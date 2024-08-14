{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}

import Data.Kind (Type)
import Data.Proxy 
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits hiding ((+))
import Unsafe.Coerce
import Fcf hiding (Any)
import Data.Functor.Identity

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

data Key (key :: Symbol) = Key

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

insert :: Eval (UniqueKey key ts) ~ True =>
  Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) : ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type UniqueKey (key :: k) (ts :: [(k, t)]) = Null =<< Filter (TyEq key <=< Fst) ts

type MaybeFindElem (key :: Symbol) (ts :: [(Symbol, t)]) = FindIndex (TyEq key <=< Fst) ts

type FindElem (key :: Symbol) (ts :: [(Symbol, t)]) = Eval (FromMaybe Stuck =<< MaybeFindElem key ts)

type Member (key :: Symbol) (ts :: [(Symbol, t)]) = KnownNat (FindElem key ts)


findElem :: forall key ts. Member key ts => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< Lookup key ts

get :: forall key ts f. Member key ts =>
  Key key -> OpenProduct f ts -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
  where 
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. Member key ts
       => Key key -> f t -> OpenProduct f ts -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) = Filter (Not <=< TyEq key <=< Fst) ts

delete :: forall key ts f. Member key ts => 
  Key key -> OpenProduct f ts -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) = OpenProduct $ V.ifilter (\i _ -> i /= findElem @key @ts) v

instance (key ~ key') => IsLabel key (Key key') where
  fromLabel = Key

main = do
  let test = insert (Key @"nome") (Identity "Carlos") $ insert (Key @"idade") (Identity 3) (nil @Identity)
  print (get #idade test)
  print (get #nome test)
  -- print (get #a test) -- erro em tempo de compilação (Não há campo chamado "a")