module MonadicWay where

newtype Lover a = Lover {loverDiary :: (Name,a)}

type Name = String

createLover :: Name -> a -> Lover a
createLover name times = Lover (name,times)

startAffairWith :: forall t t1. (Num t1) => Name -> Lover t -> Lover t1
startAffairWith name (Lover _ ) = Lover (name, 0)
                                             