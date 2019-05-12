-- 2 Dictionary Implementation
-- Create a data type with Empty and List (a, b) (Dict a b) constructors
data Dict a b = Empty | List (a, b) (Dict a b) deriving (Eq, Show)

-- return an empty dictionary
emptyDict :: Dict a b
emptyDict = Empty


-- check if there's a given key in a dictionary recursively.
hasKey :: Eq a => Dict a b -> a -> Bool
hasKey Empty x = False
hasKey (List (a, b) rest) x | a == x = True
                             | otherwise = hasKey rest x

-- get the value of a given key in a dictionary recursively.
getValue :: Eq a => Dict a b -> a -> b
getValue (List (a, b) rest) x | a == x = b
                              | otherwise = getValue rest x

-- add a new key-value pair to a dictionary recursively.
withKeyValue :: Eq a => Dict a b -> a -> b -> Dict a b
withKeyValue Empty a b = List (a, b) Empty
withKeyValue (List (a, b) rest) x y = List (x, y) (List (a, b) rest)

-- remove a key-value pair from a dictionary recursively.
withoutKey :: Eq a => Dict a b -> a -> Dict a b
withoutKey (List (a, b) rest) x | x == a = rest
                                | otherwise = List (a, b) (withoutKey rest x)

-- Way to remove item by transform Dict to list
dictToList :: Eq a => Dict a b -> [(a, b)]
dictToList (List (a, b) rest) = (a, b):dictToList(rest)
listToDict :: Eq a => [(a, b)] -> Dict a b
listToDict [] = Empty
listToDict ((a, b):rest) = List (a, b) (listToDict rest)
removedList :: Eq a => [(a, b)] -> a -> [(a, b)]
removedList ((a, b):rest) x | x == a = rest
                            | otherwise = (a, b):removedList rest x

-- 2.1 Map Application
-- Demonstrate the use of your map data type with an example
-- and make sure to demonstrate two useful applications of currying
-- in conjunction with the Dict datatype.

-- here is the function that works like map with Dict datatype, so it is Dict data type instead of list.
-- We are using SDict because it looks cleaner
myMap :: (b->b) -> SDict a b -> SDict a b
myMap f (SList (a, b) SEmpty) = SList (a, (f b)) SEmpty
myMap f (SList (a, b) rest) = SList (a, (f b)) (myMap f rest)

a = emptySDict
b = withKeyValueSDict a 3 5
c = withKeyValueSDict b 5 10
d = withKeyValueSDict c 8 12
-- currying
e = myMap add5 d
-- d and e in GHCI to have a look at results

-- currying, accepting different implementations
total1 :: Num b => [(a, b)] -> b
total1 [] = 0
total1 ((a, b):xs) = b + total1 xs

total2 :: (Eq a, Num b) => SDict a b -> b
total2 SEmpty = 0
total2 (SList (a, b) rest) = b + total2 rest

total :: Eq a => (a->b) -> a -> b
total f a = f a
-- f and g in GHCI to have a look at results
f = total total1 [(1, 2), (2, 3)]
g = total total2 e

-- function for demonstrate myMap function for currying.
add5 :: Num b => b -> b
add5 b = b + 5

-- 2.2 Bonus
-- ascending order with key by checking the position to add a pair.
-- Example:
as = emptySDict
bs = withKeyValueSDict as 3 5
cs = withKeyValueSDict bs 7 10
ds = withKeyValueSDict cs 5 8
-- print ds
-- or
-- ds
-- in GHCI
-- Result: SList (3,5) (SList (5,8) (SList (7,10) SEmpty))

data SDict a b = SEmpty | SList (a, b) (SDict a b) deriving (Eq, Ord, Read, Show)

-- return an empty dictionary
emptySDict :: SDict a b
emptySDict = SEmpty


-- check if there's a given key in a dictionary recursively.
hasKeySDict :: (Eq a, Ord a) => SDict a b -> a -> Bool
hasKeySDict SEmpty x = False
hasKeySDict (SList (a, b) rest) x | a == x = True
                                  | otherwise = hasKeySDict rest x

-- get the value of a given key in a dictionary recursively.
getValueSDict :: (Eq a, Ord a) => SDict a b -> a -> b
getValueSDict (SList (a, b) rest) x | a == x = b
                                    | otherwise = getValueSDict rest x

-- add a new key-value pair to a dictionary recursively.
-- only add at the position where the key is smaller.
withKeyValueSDict :: (Eq a, Ord a) => SDict a b -> a -> b -> SDict a b
withKeyValueSDict SEmpty a b = SList (a, b) SEmpty
withKeyValueSDict (SList (a, b) rest) x y | x < a = SList (x, y) (SList (a, b) rest)
                                          | otherwise = SList (a, b) (withKeyValueSDict rest x y)

-- remove a key-value pair from a dictionary recursively.
withoutKeySDict :: (Eq a, Ord a) => SDict a b -> a -> SDict a b
withoutKeySDict (SList (a, b) rest) x | x == a = rest
                                      | otherwise = SList (a, b) (withoutKeySDict rest x)
