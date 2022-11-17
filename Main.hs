-- Author: Joshua Quinto, jquinto2018@my.fit.edu
-- Author: Robert Heine, rheine2019@my.fit.edu

module Main where

-- Data Structure for the Binary tree
-- Each node is either a 'Node', with no data and two children,
--  or a 'Leaf', which holds just a letter and has no children
data BinTree = Node BinTree BinTree
    | Leaf Char
    deriving Show

-- Takes in a string of characters (the tree encoding) and then outputs a tuple
-- with the tree in fst.
-- Second item in the tuple is used to properly update the recursive function
-- with the remaining encoding string after a given subtree is constructed.
-- Without it, the tree would be constructed incorrectly and contain duplicates
-- of almost every node.
build_tree :: [Char] -> (BinTree, String)
-- Base case, last element in encoding string will always be a letter.
build_tree [x] = (Leaf x, "end")
-- Main recursive function
build_tree (x:xs)
    | x == '*' = -- case for building a Node
        let recur = build_tree xs -- calculates left subtree
            recur2 = build_tree (snd(recur)) -- calculates right subtree
        in (Node (fst(recur)) (fst(recur2)), (snd(recur2)))
    | otherwise = (Leaf x, xs) -- case for building a Leaf

-- Takes in a binary string, aka the query, and returns the string from the tree
-- Second BinTree argument is used to return to the root of the tree when a
-- letter is found.
decode :: String -> BinTree -> BinTree -> [Char]
-- Case where the last letter in the tree has been reached
decode [] (Leaf c) _ = [c]
-- Handles one character left in the query
decode [x] (Node l r) root
    | x == '0' = decode [] l root
    | x == '1' = decode [] r root
-- Case where we have reached a letter and need to return to root
decode (x:xs) (Leaf c) root = [c] ++ (decode (x:xs) root root)
-- Letter has not been reached, so continues down the tree
decode (x:xs) (Node l r) root
    | x == '0' = decode xs l root
    | x == '1' = decode xs r root

-- Input Output
main :: IO ()
-- Feeds each query into evalQueries, which does as the name suggests
main = interact (eachLine evalQueries)

-- Separates input into separate lines, then rebuilds the output.
eachLine :: (String -> BinTree -> String) -> (String -> String)
eachLine f = unlines . dispatch f . lines

-- Separates the first line of input and builds the tree from it.
-- Passes remaining lines to dispatch2 to evaluate as queries.
dispatch :: (String -> BinTree -> String) -> [String] -> [String]
dispatch f (l:ls) = let tree = fst(build_tree l) in dispatch2 f ls tree

-- Maps evalQueries with the tree and the varying lines from input
-- to evaluate each query line by line.
dispatch2 :: (String -> BinTree -> String) -> [String] -> BinTree -> [String]
dispatch2 f ls t = map (\l -> f l t) ls

-- Evaluates a given query and returns it.
evalQueries :: String -> BinTree -> String
evalQueries q t = decode q t t
