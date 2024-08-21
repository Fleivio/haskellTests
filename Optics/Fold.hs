
type Fold s a = forall f. (Applicative f) => (a -> f a) -> s -> f s

toListOf :: Fold s a -> s -> [s]
toListOf fl s = fl pure s

