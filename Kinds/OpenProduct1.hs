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
import GHC.TypeLits hiding ((+))
import GHC.OverloadedLabels (IsLabel(..))
import Unsafe.Coerce
import Fcf

data Forgotten where
  Forget :: a -> Forgotten

type Open :: [(Symbol, t)] -> Type
data Open ts where
  Open :: [Forgotten] -> Open ts

nil :: Open '[]
nil = Open []

data Key (s :: Symbol) = Key

instance (k ~ s) => IsLabel k (Key s) where
  fromLabel = Key

type MElemType (k :: Symbol) (ts :: [(Symbol, t)]) = Lookup k ts
type ElemType (k :: Symbol) (ts :: [(Symbol, t)]) 
  = FromMaybe 
    (TypeError 
    (Text "O campo definido pelo símbolo" ':$$: ShowType k ':$$: Text " não existe em Open")
    ) 
  =<< MElemType k ts

type MElemIndex (k :: Symbol) (ts :: [(Symbol, t)]) = FindIndex (TyEq k <=< Fst) ts
type ElemIndex (k :: Symbol) (ts :: [(Symbol, t)]) = FromMaybe Stuck =<< MElemIndex k ts

type HasKey (k :: Symbol) (ts :: [(Symbol, t)]) = IsJust =<< Find (TyEq k <=< Fst) ts

type Member (k :: Symbol) (ts :: [(Symbol, t)]) = KnownNat (Eval (ElemIndex k ts))
type NotMember (k :: Symbol) (ts :: [(Symbol, t)]) = Eval (HasKey k ts) ~ False

type DelKey (k :: Symbol) (ts :: [(Symbol, t)]) = Filter (Not <=< TyEq k <=< Fst) ts

type UpdateType (k :: Symbol) (h :: t) (ts :: [(Symbol, t)]) = SetIndex (Eval (ElemIndex k ts)) '(k,h) ts 

type InsOrUp (k :: Symbol) (h :: t) (ts :: [(Symbol, t)]) 
  = FromMaybe ('(k,h) ': ts) =<< Map (Flip2 SetIndex '(k,h) ts) =<< FindIndex (TyEq k <=< Fst) ts

data Flip2 :: (a -> b -> c -> Exp r) -> b -> c -> a -> Exp r
type instance Eval (Flip2 f b c a) = Eval (f a b c)

insert :: forall k ts t. NotMember k ts =>
  Key k -> t -> Open ts -> Open ('(k,t) : ts)
insert _ x (Open xs) = Open (Forget x : xs)

getIndex :: forall k ts. Member k ts => Int
getIndex = fromIntegral . natVal $ Proxy @(Eval (ElemIndex k ts))

get :: forall k ts. Member k ts =>
  Key k -> Open ts -> Eval (ElemType k ts)
get _ (Open ts) = remember $ ts !! getIndex @k @ts
  where
    remember (Forget a) = unsafeCoerce a

delete :: forall k ts. Member k ts =>
  Key k -> Open ts -> Open (Eval (DelKey k ts))
delete _ (Open xs) = let (a,b) = splitAt (getIndex @k @ts) xs
                     in Open $ a ++ drop 1 b

update :: forall k ts t. Member k ts =>
  Key k -> t -> Open ts -> Open (Eval (UpdateType k t ts))
update _ x (Open xs) = let (a,b) = splitAt (getIndex @k @ts) xs
                       in Open $ a ++ [Forget x] ++ drop 1 b


class FindInsOrUpElem (a :: Maybe Nat) where
  insOrUp :: Maybe Int

instance KnownNat a => FindInsOrUpElem (Just a) where
  insOrUp = Just . fromIntegral . natVal $ Proxy @a

instance FindInsOrUpElem Nothing where
  insOrUp = Nothing 

upsert :: forall k t ts. FindInsOrUpElem (Eval (MElemIndex k ts))
  => Key k -> t -> Open ts -> Open (Eval (InsOrUp k t ts))
upsert _ x (Open xs) = Open $ case insOrUp @(Eval (MElemIndex k ts)) of
  Nothing -> Forget x : xs 
  Just n  -> let (a,b) = splitAt n xs
             in a ++ [Forget x] ++ drop 1 b


main = do
  let test = insert #nome "Carlos" $
             insert #idade 30
             nil
  print (get #idade test)
  print (get #nome test)

  -- print (get (Key @"trabalho") test) -- erro em tempo de compilação
  