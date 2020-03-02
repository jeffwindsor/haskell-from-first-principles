module HFFP.Ch6 where

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False

data Date =
  Date DayOfWeek Int

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
             weekday == weekday'
          && dayOfMonth == dayOfMonth'

-----------------------------------------
-- Write the equality for the following (p.181)
--1. It’s not a typo, we’re just being cute with the name:
newtype TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') =
        x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y) = x == y
    (==) (TisAString a) (TisAString b) = a == b

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') =
        x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') =
        x == x' && y == y'

data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne a) (ThatOne b) = a == b

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye a) (Goodbye b) = a == b


-- Chapter Exercises  p.208
--  Multiple Choice: 1.c; 2.a,b 3.a; 4.c; 5.a;

data Mood = Blah | Woot
    deriving (Show, Eq)
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq,Show)

newtype Rocks = Rocks String deriving (Eq, Show)
newtype Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

--Type-Kwon-Do Two: Electric typealoo
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a * fromInteger i

class Eq' a where
  (*==) :: a -> a -> Bool
  (*/=) :: a -> a -> Bool
  -- let's just implement one function in terms of the other
  x */= y = not (x *== y)