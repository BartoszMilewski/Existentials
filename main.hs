{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

{-
   In logic, these two statements are equivalent:

   (1) (exists b.  P(b)) => Q
   (2)  forall b. (P(b) => Q)

   (1) If there exists such b that the proposition P(b) is true then Q is true
   (2) For any b, if P(b) is true then Q is true

   Q does not depend on b!

   Using Curry-Howard isomorphism this would translate to 
   (if we had exists in the language):

   The existence of a function:
      (exists b. P b) -> Q

   is equivalent to the existence a polymorphic function:
      forall b. P b -> Q

   (Remember, under C-H isomorphism, propositions become types,
    implications become function types, and
    a statment is true if the corresponding type is inhabited.)

   In other words:
   (1) If there is a type b for which you can construct
       an element of the type P b (i.e., P b is inhabited),
       then I can construct for you the type Q (Q is inhabited).
   (2) for all types b, I can construct a function from P b to Q
   So, if Q is a type, the proof of inhabitance can be written as 
   a constructor for this type that takes the element of the type P b.
-}

{-
  Example: Q is the Stream type, P b is 
  a conjunction (tuple) of three types: b, b->a, and b->b.
  Both types are additionally parameterized by type a.
  The interpretation: b is an implementation type 
  (for instance, an infinite list of a), b->a extracts
  an element from the stream, and b->b advances the stream.

  In existential notation we would translate (1) to:

  data Stream a where 
     S (exists b. b (b->a) (b->b))

  This following is equivalent to (2):
-}

data Stream a = forall b. S b (b->a) (b->b)

-- This is still clearer in GADT notation
-- Notice that the universal quantifier is implicit
-- for any type argument that is not mentioned in
-- the head of the definition — here it’s b

data Stream' a where
    S' :: b -> (b->a) -> (b->b) -> Stream' a

-- or, uncurried

data Stream'' a where
    S'' :: (b, b->a, b->b) -> Stream'' a

-- We can construct such stream, for instance, from [Int]

strm :: Stream Int
strm = S [0..] head tail

getScnd :: Stream Int -> Int
getScnd (S b extract advance) = extract (advance b)

-- Notice that the client has no idea what the type of b is

-- We can also use another implementation:

strm2 :: Stream Int
strm2 = S 0 id (+1)

-- The function getScnd will work for strm2 without any change

-- Here’s another example:

data Stringy = forall a. Show a => MkStringy a

instance Show Stringy where
    show (MkStringy x) = show x

lst :: [Stringy]
lst = [MkStringy 'a', MkStringy (10::Int), MkStringy 3.14]

main = do
  print $ getScnd strm
  print $ getScnd strm2
  print lst