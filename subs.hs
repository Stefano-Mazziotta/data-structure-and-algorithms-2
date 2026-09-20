
-- how to run this program using ghci
-- Save the code in a file named subs.hs
-- Open a terminal and navigate to the directory where subs.hs is located
-- Run the command: ghci subs.hs
-- This will load the file into GHCi
-- Once in GHCi, you can run the main function by typing: main
-- Press Enter to execute the command and see the output

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

main :: IO ()
xs = [1..3]
main = print (subs xs)


-- Output: ["","c","b","bc","a","ac","ab","abc"]

-- how to 
