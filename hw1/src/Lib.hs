module Lib where

import Data.Function

-- 1

-- 1.1
{-
((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))
((λ p. (λ q. ((q (p r)) s))) ((q (r)) s))
((λ p. (λ q. ((q (p r)) s))) ((q r) s))
((λ p. (λ q. ((q (p r)) s))) (q r s))
(λ p. (λ q. q (p r) s)) (q r s)
(λ p. (λ Q. Q (p r) s)) (q r s)
(λ P.λ Q. Q (P r) s) (q r s)
(λ Q. Q ((q r s) r) s) 
(λ Q. Q (q r s r) s)
λ Q. Q (q r s r) s
-}

-- 1.2
{-
((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]
((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ Z. Z)) [x := b]
((λ a. λ b. b a (a b x) (λ b. x)) (λ Z. Z)) [x := b]
(λ b. b (λ Z. Z) ((λ Z. Z) b x) (λ b. x)) [x := b]
(λ b. b (λ Z. Z) (b x) (λ b. x)) [x := b]
(λ b. b (λ Z. Z) (b x) (λ Q. x)) [x := b]
(λ b. b (λ Z. Z) (b x) (λ Q. x)) [x := b]
(λ P. P (λ Z. Z) (P x) (λ Q. x)) [x := b]
(λ P. P (λ Z. Z) (P b) (λ Q. b))
-}

-- 2.1 
distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left a) = (Left a, Left a)
distributivity (Right (b, c)) = (Right b, Right c)

-- 2.2
associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (a, (b, c)) = ((a, b), c)

-- 2.3 FIXME
-- type (<->) a b = (a -> b, b -> a)

-- eitherAssoc
--   :: Either a (Either b c)
--   <-> Either (Either a b) c
-- eitherAssoc (Left a) = Left (Left a)
-- eitherAssoc (Right (Left b)) = Left (Right b)
-- eitherAssoc (Right (Right c)) = Right c

-- 2.4
pairProd
  :: (a -> b)
  -> (c -> d)
  -> (a,c)
  -> (b,d)
pairProd f g (x, y) = (f x, g y)

-- 2.5
weirdFunction
  :: (d -> d -> b)
  -> (a -> b -> c)
  -> (d -> b)
  -> d -> b
-- weirdFunction _ _ f d = f d
weirdFunction f _ _ d = f d d

-- 2.6
eitherAssoc
  :: Either a (Either b c)
  -> Either (Either a b) c
eitherAssoc (Left a) = Left (Left a)
eitherAssoc (Right (Left b)) = Left (Right b)
eitherAssoc (Right (Right c)) = Right c

-- 2.7
distr
  :: (a -> b -> c)
  -> (a -> b)
  -> a -> c
distr f g a = f a (g a)

-- --- -- 
--     -- 
--  3  -- 
--     -- 
-- --- --

data Nat = Zero | Succ Nat

-- 3.1
toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ prev) = toInt prev + 1

instance Show Nat where
  show Zero = "0"
  show x = show $ toInt x

-- 3.2
instance Eq Nat where
  Zero == Zero     = True
  _    == Zero     = False
  Zero == _        = False
  Succ x == Succ y = x == y

-- 3.3
instance Ord Nat where
  compare Zero Zero         = EQ
  compare _ Zero            = GT
  compare Zero _            = LT
  compare (Succ x) (Succ y) = compare x y

-- 3.4
instance Num Nat where
  (+) x Zero     = x
  (+) x (Succ y) = Succ x + y

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) x (Succ Zero) = x
  (*) x (Succ y)    = x * y + x

  (-) Zero y            = error "Natural numbers are nonnegative"
  (-) x Zero            = x
  (-) (Succ x) (Succ y) = x - y

  abs x = x
  signum Zero = 0
  signum _    = 1

  fromInteger x
      | x == 0 = Zero
      | x > 0  = Succ $ fromInteger (x - 1)
      | otherwise = error "Natural numbers are nonnegative" 

-- 3.5
instance Enum Nat where
  toEnum   = undefined
  fromEnum = undefined


-- --- -- 
--     -- 
--  4  -- 
--     -- 
-- --- --

iterateElement :: a -> [a]
iterateElement x = fix (x:)

fibonacci :: Integer -> Integer
fibonacci = fix (\rec x -> case x of
    0 -> 0
    1 -> 1
    _ -> rec (x-1) + rec (x-2)
    )

factorial :: Integer -> Integer
factorial = fix (\rec x -> if x == 0 then 1 else x * rec (x - 1))

mapFix :: (a -> b) -> [a] -> [b]
mapFix f = fix (\rec (x:xs) -> (f x: rec xs))


-- --- --
--     --
--  5  --
--     --
-- --- --

-- 5.1
{-
distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

Для аргумента Left _ у нас фукнция определена как 
distributivity (Left a) = (Left a, Left a)

То есть, если я правильно понимаю, то самое внешнее выражение - конструктор пары (,), то есть
(Left ("harold" ++ " hide " ++ "the " ++ "pain"), Left ("harold" ++ " hide " ++ "the " ++ "pain"))

Для проверки в ghci попробовал ввести
> x = distributivity (Left undefined)
ошибки не вызвало, а значит и внутренности не вычисляются сразу
-}

-- 5.2
{-

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True -> Just $ exp pi
    False -> Nothing

null $ mapMaybe foo "pole chudes ochen' chudesno"

> x = null $ mapMaybe undefined undefined

-}


-- --- --
--     --
--  6  --
--     --
-- --- --

type NatChurch a = (a -> a) -> a -> a

zero :: NatChurch a
zero f x = x

-- 6.1
succChurch :: NatChurch a -> NatChurch a
succChurch n f x = f (n f x)

-- 6.2
churchPlus :: NatChurch a -> NatChurch a -> NatChurch a
churchPlus n m f x = m f (n f x)

-- 6.3
churchMult :: NatChurch a -> NatChurch a -> NatChurch a
churchMult n m f = m (n f)

-- 6.4
churchToInt :: NatChurch Integer -> Integer
churchToInt f = f ( (+) 1 ) 0

-- --- --
--     --
--  7  --
--     --
-- --- --

-- --- --
--     --
--  8  --
--     --
-- --- --

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- 8.1
instance Show a => Show (Tree a) where
  show Leaf = "_"
  show (Node l a r) = "(" ++ (show l) ++ (show a) ++ (show r) ++ ")"

-- 8.2
instance Eq a => Eq (Tree a) where
  Leaf == Leaf = True
  Leaf == _ = False
  _ == Leaf = False
  (Node l1 x1 r1) == (Node l2 x2 r2) = (l1 == l2) && (x1 == x2) && (r1 == r2)

-- 8.3
treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node left x right) = (treeToList left) ++ [x] ++ (treeToList right)

-- 8.4
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- 8.5
nodesNum :: Tree a -> Int
nodesNum Leaf = 0
nodesNum (Node left x right) = (nodesNum left) + 1 + (nodesNum right)

-- --- --
--     --
--  9  --
--     --
-- --- --

-- 9.1
smartReplicate [] = []
smartReplicate (x:xs) = 
    let f x' cnt xs' = if cnt == 0 then smartReplicate xs' else x':(f x' (cnt - 1) xs')
    in f x x xs

-- 9.2
containsSingle _ [] = False
containsSingle a (x:xs) = if x == a then True else containsSingle a xs

contains _ [] = []
contains a (x:xs) = if containsSingle a x then x:contains a xs else contains a xs

-- 9.3
stringSum s = foldl (\acc y -> acc + (read y :: Integer)) 0 $ words s 

-- 9.4
merge ([], x) = x
merge (x, []) = x
merge (x:xs, y:ys) = if x < y then x:merge (xs, y:ys) else y:merge (x:xs, ys)

split [] = ([], [])
split [a] = ([a], [])
split (x:y:latter) = (\(as, bs) a b -> (a:as, b:bs)) (split latter) x y

mergeSort [] = []
mergeSort [a] = [a]
mergeSort x = merge $ (\(a, b) -> (mergeSort a, mergeSort b)) $ split x


-- ---- -- 
--      -- 
--  10  -- 
--      -- 
-- ---- --

-- 10.1
data Colour = Red | Blue | Purple | Green
  deriving Show

stringToColour :: String -> Colour
stringToColour "Red"    = Red
stringToColour "Blue"   = Blue
stringToColour "Purple" = Purple
stringToColour "Green"  = Green

-- 10.2
data LogLevel = Error | Warning | Info
  deriving Show

instance Eq LogLevel where
    Error == Error = True
    Warning == Warning = True
    Info == Info = True
    _ == _ = False

instance Ord LogLevel where
  compare Error Error = EQ
  compare Error _     = GT
  compare _ Error     = LT

  compare Warning Warning = EQ

  compare Info Info = EQ
  compare Info _    = LT
  compare _ Info    = GT

-- 10.3
data Sex = Male | Female
  deriving (Show, Eq)

data Person
  = Person
  { firstName :: String
  , lastName  :: String
  , age :: Int
  , sex :: Sex
  }

updateLastName
  :: Person
  -> Person
  -> Person
updateLastName (Person _ ln_p1 _ sex_p1) (Person fn_p2 _ age_p2 _)
    = Person fn_p2 ln_p1 age_p2 sex_p1 

