
import System.IO  
import Prelude
import Data.List

whitespaces :: String
whitespaces = " \n\r\t\f\v"

stripChars :: String -> String -> String
stripChars = filter . flip notElem

stripWhiteSpaces :: String -> String
stripWhiteSpaces = stripChars whitespaces

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

isRectangle :: [String] -> Bool
isRectangle contents = allEqual content_lengths
    where content_lengths = map length contents

findPlayerPos :: Board -> (Int,Int)
findPlayerPos board = head $ [ (i,j) | (row, i) <- zip board [0..] ,  j <- elemIndices ball row ]

type Item = Char
type Board = [[Item]]


boardToStr :: Board -> String
boardToStr board = unlines (map (intersperse ' ') board)

-- ‘@’ represents the ball.
ball::Item
ball = '@'
-- ‘-’ represents a path block that the ball could roll on.
path :: Item 
path = '-'
-- ’*’ represents the grass (obstacles).
grass :: Item
grass = '*'
-- ‘p’ represents the tile of the path’s block color is the special color : pink.
pink :: Item
pink = 'p'
-- ‘o’ represents the tile of the path’s block color is the special color : orange.
orange :: Item 
orange = 'o'
-- ‘y’ represents the tile of the path’s block color is the special color : yellow.
yellow :: Item 
yellow = 'y' 
-- ‘b’ represents the bonus (the stars in Kodable).
bonus :: Item
bonus = 'b'
-- ‘t’ represents the target point.
target :: Item
target = 't'

data GameMap = GameMap 
               { getBoard :: Board ,
                 getHeight :: Int,
                 getWidth :: Int,
                 getPlayerPos :: (Int,Int), 
                 playerWon :: Bool
                }  

charToItem :: Char -> Item 
charToItem x 
    | x == ball = ball 
    | x == path = path 
    | x == grass = grass 
    | x == pink = pink 
    | x == yellow = yellow 
    | x == orange = orange 
    | x == bonus = bonus 
    | x == target = target 
    | otherwise = grass


stringToItems :: String -> [Item]
stringToItems [] = []
stringToItems (x:xs) = [(charToItem x)] ++ stringToItems xs 



kodableEmptyMap :: IO () 
kodableEmptyMap = kodable "poopster"

kodable :: String -> IO () 
kodable des = do 
    putStr ">"
    command <- getLine
    let command_list = words command
    if head command_list == "quit"
        then do return ()
    else if (head command_list == "load" && (length command_list == 2))
        then do gamemap <- loadFile (command_list !! 1)
                des <- getLine
                putStr (boardToStr $ getBoard gamemap)
                kodable des
    else if head command_list == "play"
        then do direction_list <- play [] 
                putStrLn (show direction_list)
    else do kodable des


directions :: [String]
directions = ["Left","Right","Up","Down"]



getPlayDirection :: [String] -> IO [String]
getPlayDirection xs = do command <- getLine
                         let stripped_command = stripWhiteSpaces command -- take away trailing whitespaces
                         if (not $ null command) && (not $ null $ elemIndices stripped_command directions) 
                            then do play (xs ++ [stripped_command])
                         else do return xs

-- invalid input will cause the play IO to return 
play :: [String] -> IO [String]
play xs = do if null xs
                then do putStr "First   Direction : "
                        getPlayDirection xs
             else do putStr "Next    Direction : "
                     getPlayDirection xs




loadFile :: FilePath -> IO GameMap
loadFile filepath = do        
    handle <- openFile filepath ReadMode  
    contents <- hGetContents handle
    let cleanboard = map stringToItems (map stripWhiteSpaces (lines contents))
        boardHeight = length cleanboard
        boardWidth = length (head cleanboard)
        playerPos = findPlayerPos cleanboard
        new_map = GameMap {getBoard=cleanboard, 
                           getHeight=boardHeight, 
                           getWidth=boardWidth, 
                           getPlayerPos = playerPos,
                           playerWon = False}
    -- check if map is solvable and is rectangular
    putStrLn "Read map successfully!"
    putStrLn (show cleanboard)
    putStrLn (boardToStr cleanboard)
    putStrLn (show playerPos)
    hClose handle  
    return new_map


sample_board :: Board


-- TODO will delete
test = do xs <- play []
          putStrLn $ show xs


getItemFromPos :: Board -> Int -> Int -> Item 
getItemFromPos board i j = (board !! i) !! j





-- testDirection :: GameMap -> String -> Bool  
-- testDirection game_map direction 
--         | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) /= grass)
--         | direction == "Right" = (j<width) && (getItemFromPos board (j+1) /= grass)
--         | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j /= grass) 
--         | direction == "Down" = (i<height) && (getItemFromPos board (i+1) j /= grass)
--         where (i,j) = getPlayerPos game_map 
--               board = getBoard game_map
--               width = getWidth game_map 
--               height = getHeight game_map


-- moveOneStep :: GameMap -> String -> GameMap  
-- moveOneStep game_map direction 
--         | (direction == "Left") && tested = GameMap { get}
--         | (direction == "Right") && tested = 
--         | (direction == "Up") && tested =
--         | (direction == "Down") && tested = 
--         where (i,j) = getPlayerPos game_map 
--               board = getBoard game_map
--               tested = testDirection game_map direction



              

sample_board = ["*****-------------------*****",
                "*****b-----------------b*****",
                "*****-*****************-*****",
                "*****-**----*****----**-*****",
                "*****-**-yy-*****-yy-**-*****",
                "*****-**----*****----**-*****",
                "*****-******--b--******-*****",
                "*****-******-***-******-*****",
                "@-----******-***-******p----t",
                "*****-******-***-******-*****",
                "*****--------***--------*****",
                "*****************************",
                "*****************************"]
