module Squeal.PostgreSQL.Boilerplate where

import Control.Lens
import Data.Proxy
import Data.SOP qualified as SOP
import Data.Singletons
import Data.Singletons.TH (genSingletons)
import GHC.TypeLits
import Squeal.PostgreSQL

$(genSingletons [''ReferentialAction, ''OnDeleteClause, ''OnUpdateClause])

--------------------------------------------------------------------------------
-- Column type expression lists

class DefaultColumnTypeExpressions db cols where
  defaultColumnTypeExpressions :: NP (Aliased (ColumnTypeExpression db)) cols

instance
  ( DefaultColumnTypeExpression db (ColumnTypeExpressionMode x)
  , KnownSymbol k
  , DefaultColumnTypeExpressions db xs
  )
  => DefaultColumnTypeExpressions db ('(k, x) ': xs)
  where
  defaultColumnTypeExpressions =
    (defaultColumnTypeExpression `as` fromLabel @k) :* defaultColumnTypeExpressions

instance DefaultColumnTypeExpressions db '[] where
  defaultColumnTypeExpressions = Nil

----------------------------------------
-- Column type expressions

type family ColumnTypeExpressionMode t = (x :: CTE t) | x -> t where
  ColumnTypeExpressionMode (Def :=> NotNull PGint4) = CTE'Serial
  ColumnTypeExpressionMode (Def :=> NotNull t) = CTE'NotNullDef
  ColumnTypeExpressionMode (Def :=> Null t) = CTE'NullDef
  ColumnTypeExpressionMode (NoDef :=> NotNull t) = CTE'NotNull
  ColumnTypeExpressionMode (NoDef :=> Null t) = CTE'Null

data CTE (t :: ColumnType)
  = CTE'Serial
  | CTE'NotNull
  | CTE'NotNullDef
  | CTE'Null
  | CTE'NullDef

class (ColumnTypeExpressionMode ty ~ mode) => DefaultColumnTypeExpression (db :: SchemasType) (mode :: CTE ty) where
  defaultColumnTypeExpression :: ColumnTypeExpression db ty

instance DefaultColumnTypeExpression db (CTE'Serial @(Def :=> NotNull PGint4)) where
  defaultColumnTypeExpression = serial

instance
  ( ColumnTypeExpressionMode col ~ CTE'NotNullDef
  , DefaultExpression (NotNull ty)
  , PGTyped db ty
  , col ~ Def :=> NotNull ty
  )
  => DefaultColumnTypeExpression db (CTE'NotNullDef @col)
  where
  defaultColumnTypeExpression = default_ defaultExpression (notNullable pgtype)

instance
  ( ColumnTypeExpressionMode col ~ CTE'NotNull
  , PGTyped db ty
  , col ~ NoDef :=> NotNull ty
  )
  => DefaultColumnTypeExpression db (CTE'NotNull @col)
  where
  defaultColumnTypeExpression = notNullable pgtype

instance
  ( ColumnTypeExpressionMode col ~ CTE'Null
  , PGTyped db ty
  , col ~ NoDef :=> Null ty
  )
  => DefaultColumnTypeExpression db (CTE'Null @col)
  where
  defaultColumnTypeExpression = nullable pgtype

--------------------------------------------------------------------------------
-- Constraint expression lists

type family ForeignKeyBehaviour (db :: SchemasType) (tbl :: Symbol) (fk :: Symbol) :: (OnDeleteClause, OnUpdateClause)

class DefaultTableConstraintExpression sch tab db name constraint where
  defaultTableConstraintExpression :: Aliased (TableConstraintExpression sch tab db) '(name, constraint)

instance
  ( ForeignKeyBehaviour db tab name ~ '(onDelete, onUpdate)
  , SingI onDelete
  , SingI onUpdate
  , Has sch0 db schema0
  , Has sch1 db schema1
  , Has child schema1 ('Table tab)
  )
  => DefaultTableConstraintExpression sch1 tab db name ('ForeignKey columns sch0 parent refcols)
  where
  defaultTableConstraintExpression =
    foreignKey
      undefined
      QualifiedAlias
      undefined
      (demote @onDelete)
      (demote @onUpdate)
      `as` Alias

--------------------------------------------------------------------------------
-- Selection lists from schemas

type family SchemaRow (schemum :: SchemumType) :: [(Symbol, NullType)] where
  SchemaRow ('Table '(_constraints, cols)) = DropOptionalities cols
  SchemaRow ('View row) = row

type family DropOptionalities (cols :: [(Symbol, (Optionality, NullType))]) :: [(Symbol, NullType)] where
  DropOptionalities '[] = '[]
  DropOptionalities ('(colName, '(_opt, nt)) ': xs) = '(colName, nt) ': DropOptionalities xs

defaultSelectionList
  :: (SOP.All DefaultSelectColumn cols)
  => NP (Aliased (Expression grp lat with db params from)) cols
defaultSelectionList = SOP.cpara_SList (Proxy :: Proxy DefaultSelectColumn) Nil (defaultSelectColumn :*)

class DefaultSelectColumn (a :: (Symbol, NullType)) where
  defaultSelectColumn :: Aliased (Expression grp lat with db params from) a

instance
  ( KnownSymbol name
  , forall grp lat with db params from. IsLabel name (Expression grp lat with db params from nullty)
  )
  => DefaultSelectColumn '(name, nullty)
  where
  defaultSelectColumn = As (fromLabel @name) (Alias @name)

--------------------------------------------------------------------------------

-- | Default expressions for some supported types, like @now()@ for @timestamptz@
class DefaultExpression a where
  defaultExpression :: Expression grp lat with db params from a

-- | @ now() @
instance DefaultExpression (null 'PGtimestamptz) where
  defaultExpression = now

--------------------------------------------------------------------------------
-- Lens support

-- data L :: Nat -> Type where
--   NPIndex :: (KnownNat n, n <= lim) => Proxy n -> NPIndex lim

data Peano = Zero | Succ Peano

type family NatToPeano (n :: Nat) :: Peano where
  NatToPeano 0 = Zero
  NatToPeano i = Succ (NatToPeano (i - 1))

{-# INLINE keyNP #-}
keyNP
  :: forall sym a l i x
   . (KeyNP sym l Zero ~ 'Just i, OverNPKey i l x)
  => Lens' (NP a l) (a x)
keyNP = lens (getNPKey (Proxy @i)) (setNPKey (Proxy @i))

type family ListIndex (xs :: [a]) (i :: Peano) :: a where
  ListIndex (x ': xs) Zero = x
  ListIndex (x ': xs) (Succ i) = ListIndex xs i

type family KeyNP (x :: a) (xs :: [(a, b)]) (acc :: Peano) :: Maybe Peano where
  KeyNP x '[] acc = 'Nothing
  KeyNP x ('(x, _) ': xs) acc = 'Just acc
  KeyNP x (_ ': xs) acc = KeyNP x xs (Succ acc)

class OverNPKey i l x where
  getNPKey :: Proxy i -> NP a l -> a x
  setNPKey :: Proxy i -> NP a l -> a x -> NP a l

instance (x ~ x') => OverNPKey Zero (x ': xs) x' where
  {-# INLINE getNPKey #-}
  {-# INLINE setNPKey #-}
  getNPKey _ (x :* _xs) = x
  setNPKey _ (_ :* xs) x = x :* xs

instance (OverNPKey n xs x') => OverNPKey (Succ n) (x ': xs) x' where
  {-# INLINE getNPKey #-}
  {-# INLINE setNPKey #-}
  getNPKey _ (_ :* xs) = getNPKey (Proxy :: Proxy n) xs
  setNPKey _ (y :* ys) x = y :* setNPKey (Proxy :: Proxy n) ys x
