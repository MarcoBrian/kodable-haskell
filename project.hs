
import System.IO  
import Prelude
import Parser

import Data.Char 
import Data.List


{-- Section: Command Parsers--}
colorParser :: Parser Char
colorParser = char 'p' +++ char 'o' +++ char 'y'  

directionParser :: Parser String 
directionParser = string "Right" +++ string "Left" +++ string "Up" +++ string "Down" 

numberParser :: Parser Char 
numberParser = char '1' +++ char '2' +++ char '3' +++ char '4' +++ char '5' 

loopParser :: Parser (Int, String, String)
loopParser = do string "Loop"
                char '{'
                number <- numberParser 
                char '}'  
                char '{'
                direction1 <- directionParser
                char ','
                direction2 <- directionParser
                char '}'
                return (digitToInt number,direction1,direction2)

conditionalParser :: Parser (Char,String)
conditionalParser = do string "Cond"
                       char '{'
                       color <- colorParser
                       char '}' 
                       char '{'
                       direction <- directionParser
                       char '}'
                       return (color,direction)



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

findBonusPos :: Board -> [(Int,Int)]
findBonusPos board = [ (i,j) | (row, i) <- zip board [0..] ,  j <- elemIndices bonus row ]

findTargetPos :: Board -> (Int,Int)
findTargetPos board = head $ [ (i,j) | (row, i) <- zip board [0..] ,  j <- elemIndices target row ]

findConditionalsPos :: Board -> [(Int,Int,Item)]
findConditionalsPos board = [ (i,j,orange) | (row, i) <- zip board [0..] ,  j <- elemIndices orange row ] ++
                     [ (i,j,pink) | (row, i) <- zip board [0..] ,  j <- elemIndices pink row ] ++
                     [ (i,j,yellow) | (row, i) <- zip board [0..] ,  j <- elemIndices yellow row ] 


type Item = Char
type Board = [[Item]]
type Direction = String
type Point = (Int,Int,Char)

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


conditionals :: [Item] 
conditionals = [orange,yellow,pink]

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



getPlayDirection :: [String] -> IO [Direction]
getPlayDirection xs = do command <- getLine
                         let stripped_command = stripWhiteSpaces command -- take away trailing whitespaces
                         if (not $ null command) && (not $ null $ elemIndices stripped_command directions) 
                            then do play (xs ++ [stripped_command])
                         else do return xs

-- invalid input will cause the play IO to return 
play :: [String] -> IO [Direction]
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




-- TODO will delete
test = do xs <- loadFile "map1-2.txt"
          let new_pos = moveFully xs "Right"
              new_pos2 = moveFully new_pos "Down"
              new_pos3 = moveFully new_pos2 "Right"
              new_pos4 = moveFully new_pos3 "Up"
              new_pos5 = moveFully new_pos4 "Right"
              new_pos6 = moveFully new_pos5 "Down"
              new_pos7 = moveFully new_pos6 "Right"
              new_pos8 = moveFully new_pos7 "Up"
              new_pos9 = moveFully new_pos8 "Up"
              new_pos10 = moveFully new_pos9 "Left"
              a = moveFully new_pos10 "Right"
              b = moveFully a "Left"
              c = moveFully b "Right"
              e = moveFully c "Left"
              f = moveFully e "Right"
              g = moveFully f "Left"
              h = moveFully g "Right"
              i = moveFully h "Left"
              j = moveFully i  "Right"
              k  = moveFully j "Left"
              l  = moveFully k "Right"
              m = moveFully l "Left"
              n = moveFully m "Right"
              o = moveFully n "Left"


          putStrLn (boardToStr $ getBoard o)
          putStrLn (show $ getPlayerPos o)




getItemFromPos :: Board -> Int -> Int -> Item 
getItemFromPos board i j = (board !! i) !! j


getItemFromPosTuple :: Board -> (Int,  Int) -> Item 
getItemFromPosTuple board (i,j) = if validPosition (i,j) board then (board !! i) !! j else 'X' -- X means invalid (out of bounds)



-- check if item is traversable (not grass)
testDirection :: GameMap -> Direction -> Bool  
testDirection game_map direction 
        | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) /= grass)
        | direction == "Right" = (j<width) && (getItemFromPos board i (j+1) /= grass)
        | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j /= grass) 
        | direction == "Down" = (i<height) && (getItemFromPos board (i+1) j /= grass)
        where (i,j) = getPlayerPos game_map 
              board = getBoard game_map
              width = getWidth game_map 
              height = getHeight game_map


-- check if item is conditional 
testDirectionConditional :: GameMap -> Direction -> Bool  
testDirectionConditional game_map direction 
        | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) `elem` conditionals )
        | direction == "Right" = (j<width) && (getItemFromPos board i (j+1)`elem` conditionals)
        | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j `elem` conditionals ) 
        | direction == "Down" = (i<height) && (getItemFromPos board (i+1) j `elem` conditionals )
        where (i,j) = getPlayerPos game_map 
              board = getBoard game_map
              width = getWidth game_map 
              height = getHeight game_map


updateBoard :: GameMap -> Board -> Int -> Int -> GameMap 
updateBoard game_map board new_i new_j = GameMap {getBoard = board, 
                                                  getHeight= height,
                                                  getWidth = width,               
                                                  getPlayerPos = (new_i,new_j),
                                                  playerWon = False
                                                 }
    where height = getHeight game_map
          width = getWidth game_map

-- if game across a condition then we do one more 
-- else if game come across bonus then we add the bonus to the gameMap (increment bonus point) 
-- else if game come across path then recursion 
-- else just return the game
-- dont stop when meeting conditional unless the direction is conditional. 
moveFully :: GameMap -> Direction -> GameMap
moveFully game_map direction =
    if testDirectionConditional game_map direction
        then moveOneStep game_map direction
    else if testDirection game_map direction
        then moveFully (moveOneStep game_map direction) direction
    else game_map

moveOneStep :: GameMap -> Direction -> GameMap  
moveOneStep game_map direction 
        | (direction == "Left") && tested = updateBoard game_map (moveLeft board i j) i (j-1)
        | (direction == "Right") && tested = updateBoard game_map (moveRight board i j) i (j+1)
        | (direction == "Up") && tested = updateBoard game_map (moveUp board i j) (i-1) j
        | (direction == "Down") && tested = updateBoard game_map (moveDown board i j) (i+1) j
        | otherwise = game_map
        where (i,j) = getPlayerPos game_map 
              height = getHeight game_map
              width = getWidth game_map
              board = getBoard game_map
              tested = testDirection game_map direction



moveUp :: Board -> Int -> Int -> Board
moveUp board i j = 
    let move1 = edit2DArray (i-1) j ball board
        move2 = edit2DArray i j path move1
    in move2

moveDown :: Board -> Int -> Int -> Board
moveDown board i j = 
    let move1 = edit2DArray (i+1) j ball board
        move2 = edit2DArray i j path move1
    in move2

moveRight :: Board -> Int -> Int -> Board
moveRight board i j = 
    let move1 = edit2DArray i (j+1) ball board
        move2 = edit2DArray i j path move1
    in move2

moveLeft :: Board -> Int -> Int -> Board
moveLeft board i j = 
    let move1 = edit2DArray i (j-1) ball board
        move2 = edit2DArray i j path move1
    in move2


itemIsConditional :: Item -> Bool 
itemIsConditional item = item `elem` conditionals 

itemIsGrass :: Item -> Bool 
itemIsGrass item = item == grass 

move :: Board -> (Int, Int) -> Direction -> Int -> (Int,Int,Char, Int)
move board (i,j) direction bonusCount
    | direction == "Left" && nextLeftValid && (not (itemIsGrass next_left)) = move board left direction bonusC
    | direction == "Right" && nextRightValid && (not (itemIsGrass next_right)) = move board right direction bonusC
    | direction == "Up" && nextUpValid && (not (itemIsGrass next_up) ) =  move board up direction bonusC
    | direction == "Down" && nextDownValid && (not (itemIsGrass next_down)) =  move board down direction bonusC
    | otherwise = (i,j,current_item,bonusCount)
    where current_item = getItemFromPosTuple board (i,j)
          left = (i,j-1)
          right = (i,j+1)
          up = (i-1,j)
          down = (i+1,j) 
          nextLeftValid = validPosition left board
          nextRightValid = validPosition right board 
          nextUpValid = validPosition up board 
          nextDownValid = validPosition down board 
          next_left = getItemFromPosTuple board (i,j-1)
          next_right = getItemFromPosTuple board (i,j+1)
          next_up = getItemFromPosTuple board (i-1,j)
          next_down = getItemFromPosTuple board (i+1,j)
          bonusC = (if current_item == bonus then bonusCount + 1 else bonusCount)



test2 :: IO () 
test2 = do putStr (boardToStr sample_board)
           let (i,j) = findPlayerPos sample_board 
               (i_new,j_new,che,bonus) = move sample_board (i,j) "Right" 0
               (q,w,e,r) = move sample_board (i_new,j_new) "Up" bonus
           putStrLn (show (q,w,e,r))
           return ()  

-- move :: Board -> Int -> Int -> Direction -> Board
-- move board i j direction = 
--     | direction == "Left" = moveLeft
--     | direction == "Right" = moveRight
--     | direction == "Up" = moveUp 
--     | direction == "Down" = moveDown 

-- isValidMove :: Board -> Int -> Int -> Direction -> Bool
-- isValidMove board i j direction = 
--     | direction == "Left" = validPosition (i,j-1) board
--     | direction == "Right" = validPosition (i,j+1) board
--     | direction == "Up" = validPosition (i-1,j) board 
--     | direction == "Down" = validPosition (i+1,j) board


-- moveStepWise :: Board -> Int -> Int -> Direction -> Board 
-- moveStepWise board i j direction
--     | direction == "Left" && not (isGrassPosOrOutOfRange (i,j-1,'n') board) = moveLeft board i j 
--     | direction == "Right" && not (isGrassPosOrOutOfRange (i,j+1,'n') board) = moveRight board i j 
--     | direction == "Up" && not (isGrassPosOrOutOfRange (i-1,j,'n') board) = moveUp board i j 
--     | direction == "Down" && not (isGrassPosOrOutOfRange (i+1,j,'n') board) = moveDown board i j

-- moveThrough :: Board -> Int -> Int -> Direction -> Board
-- moveThrough board i j direction
--     if isValidMove i j direction
--         then moveThrough move board i j direction
    

editRow :: Int -> a -> [a] -> [a]
editRow _ _ [] = []
editRow pos elem xs = if pos >= 0 && pos < length xs
                        then ((take pos xs) ++ [elem] ++ (drop (pos+1) xs))
                      else xs

edit2DArray :: Int -> Int -> a -> [[a]] -> [[a]]
edit2DArray row_idx col_idx elem matrix = 
    if row_idx >= 0 && row_idx < length matrix
      then let old_row = matrix !! row_idx
               new_row = editRow col_idx elem old_row
           in editRow row_idx new_row matrix 
    else []
                      
sample_board :: Board
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

sample_board2 :: Board
sample_board2= ["*****-------------------*****",
                "*****b-----------------b*****",
                "*****-*****************-*****",
                "*****-**----*****----**-*****",
                "*****-**-yy-*****-yy-**-*****",
                "*****-**----*****----**-*****",
                "*****-******--*--******p----*",
                "*****-******-***-******-***-*",
                "@-----******-***-******-----t",
                "*****-******-***-******-*****",
                "*****-------****--------*****",
                "*****************************",
                "*****************************"]


sample_board3 :: Board
sample_board3 =["*****----------*--------*****",
                "*****b-----------------b*****",
                "*****-*****************-*****",
                "*****-**----*****----**-*****",
                "*****-**-yy-*****-yy-**-*****",
                "*****-**----*****----**-*****",
                "*****-******--*--******p----*",
                "*****-******-***-******-***-*",
                "@-----******-***-*******----t",
                "*****-******-***-******-*****",
                "*****-------------------*****",
                "*****************************",
                "*****************************"]

map2_2 = ["*****-------------------*****",
          "*****------------------b*****",
          "*****-*****************-*****",
          "*****-**----*****----********",
          "*****-**----*****----**-*****",
          "*****-**----*****----**-*****",
          "*****-******--b--******-*****",
          "*****-******-***-******-*****",
          "@-----******-***-******p----t",
          "*****-******-***-******-*****",
          "*****---b----***--------*****",
          "*****************************",
          "*****************************"]



validPosition :: (Int,Int) -> Board -> Bool 
validPosition (i,j) board = (i >= 0 && i < height) && (j >= 0 && j < width)
    where height = length board
          width = length (board !! 0)



{-- Section: Check board solvability --}
traversablePosition :: (Int,Int,Char) -> [(Int,Int)] -> Board -> Bool
traversablePosition (i,j,_) visited board = isValidPos && (itemAtPos /= grass) && (not isVisited)
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 
          isVisited = (i,j) `elem` visited

isGrassPosOrOutOfRange :: (Int, Int, Char) -> Board -> Bool
isGrassPosOrOutOfRange (i,j,_) board = not isValidPos || hasGrass 
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 
          hasGrass = itemAtPos == grass

isCondPos :: (Int, Int, Char) -> Board -> Bool
isCondPos (i,j,_) board = isValidPos && hasCond 
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 
          hasCond = itemAtPos `elem` [yellow,orange,pink]


getNeighbors :: (Int,Int,Char) -> Board -> [(Int,Int,Char)]
getNeighbors pos board -- 
    | direction == 'L' = if (isGrassPosOrOutOfRange left board || isCondPos pos board) then all else [left]
    | direction == 'R' = if (isGrassPosOrOutOfRange right board || isCondPos pos board) then all else [right]
    | direction == 'U' = if (isGrassPosOrOutOfRange up board || isCondPos pos board) then all else [up]
    | direction == 'D' = if (isGrassPosOrOutOfRange down board || isCondPos pos board) then all else [down]
    | direction == 'A' = all 
    where (i,j,direction) = pos 
          left = (i, j-1,'L')
          right = (i, j+1,'R') 
          up = (i-1, j,'U')
          down = (i+1, j,'D')
          all = [left, right ,up,down]
          

-- Bread First Search Approach
bfsTraverse :: [(Int,Int,Char)] -> (Int,Int) -> [(Int,Int)] -> Board -> Bool
bfsTraverse [] _ _ _ = False
bfsTraverse ((i, j, direction):xs) end visited board  
    | ((i,j) /= end) = bfsTraverse new_queue end updated_visited board
    | otherwise = True 
   where neighbors = getNeighbors (i,j,direction) board -- LEFT, RIGHT , UP, DOWN 
         traversable_positions = filter (\pos -> traversablePosition pos visited board) neighbors
         updated_visited = visited ++ [(i,j)]
         new_queue = xs ++ traversable_positions


bfsTraversePath :: [(Int,Int,Char,Char)] -> (Int,Int) -> [(Int,Int)] -> [((Int,Int,Char,Char),(Int,Int,Char,Char))]-> Board -> [(Int,Int,Char,Char)]
bfsTraversePath ((i, j, direction,item):xs) end visited history board  
    | ((i,j) /= end) = bfsTraversePath new_queue end updated_visited updated_history  board
    | otherwise = pathConstruction (i,j,direction,item) history []
   where neighbors = getNeighbors (i,j,direction) board 
         traversable_positions = filter (\pos -> traversablePosition pos visited board) neighbors
         updated_visited = visited ++ [(i,j)]
         traversable_positions_item = [ (ii,jj,dd, getItemFromPosTuple board (ii,jj) ) | (ii,jj,dd)<- traversable_positions]
         new_queue = xs ++ traversable_positions_item
         updated_history = history ++ [ ((a,b,dir,it2),(i,j,direction,item)) | (a,b,dir,it2) <- traversable_positions_item] 





pathConstruction :: (Int,Int,Char,Char) -> [((Int,Int,Char,Char),(Int,Int,Char,Char))] -> [(Int,Int,Char,Char)] -> [(Int,Int,Char,Char)]
pathConstruction (i,j,direction,item) history route
    | (length points) > 0 = (pathConstruction (head points) history updated_route )
    | otherwise = updated_route
    where points = [(x,y,dir2,item2) | ((a,b,_,_) , (x,y,dir2, item2)) <- history, (a,b) == (i,j)]
          updated_route = route ++ [(i,j,direction,item)] 


pathToEnd :: Board -> [(Int,Int,Char,Char)]
pathToEnd board = 
    let (i,j) = findPlayerPos board 
        bonusPositions = findBonusPos board
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A', getItemFromPosTuple board (i,j) )
    in  reverse (bfsTraversePath [start_pos_direction] end_pos [] [] board)

isBoardSolvable :: Board -> Bool 
isBoardSolvable board =
    let (i,j) = findPlayerPos board 
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A')  
    in  bfsTraverse [start_pos_direction] end_pos [] board





-- Search all possible paths to complete game
searchAllPath :: Board -> (Int,Int,Char) -> [(Int,Int,Char)] -> [(Int,Int)] -> [[(Int,Int,Char)]] 
searchAllPath board (i,j,dir) paths visited 
    | ((i,j) `elem` visited) = [] 
    | (getItemFromPosTuple board (i,j) == target) =  [new_path]
    | (getItemFromPosTuple board (i,j) == bonus) = concat [ searchAllPath new_board next new_path [] | next <- traversable_positions ]
    | otherwise = concat [ searchAllPath board next new_path updated_visited | next <- traversable_positions ]
    where neighbors = getNeighbors (i,j,dir) board
          new_board = edit2DArray i j '-' board
          traversable_positions = filter (\pos -> traversablePosition pos visited board) neighbors
          new_path = paths ++ [(i,j,dir)]
          updated_visited = visited ++ [(i,j)]


withBonus :: [(Int,Int)] -> [[(Int,Int,Char)]] ->  [[(Int,Int,Char)]]
withBonus bonuses [] = [] 
withBonus bonuses (p:paths) = 
    let path_coord = [ (i,j) | (i,j,dir) <- p]
        hasAllBonus = (intersect bonuses path_coord == bonuses)
        rest_path = withBonus bonuses paths
    in if hasAllBonus then [p] ++ rest_path else [] ++ rest_path

pathWithThreeBonus :: Board -> [[(Int,Int,Char)]]
pathWithThreeBonus board = 
    let bonuses = findBonusPos board 
        (i,j) = findPlayerPos board
        all_paths = searchAllPath board (i,j,'A') [] []
    in withBonus bonuses all_paths


squashMoves :: [Char] -> [Char]
squashMoves [] = []
squashMoves [x] = [x]
squashMoves (x1:x2:xs)
    | x1==x2 = squashMoves (x2:xs)
    | otherwise = x1:squashMoves (x2:xs)


testSearchAllPath :: Board -> [String]
testSearchAllPath board =
    let (i,j) = findPlayerPos board 
        bonusPositions = findBonusPos board
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A')
        paths = searchAllPath board start_pos_direction [] []
        paths_string = [ [ dir | (x,y,dir) <- path] | path <-paths]
        result = map squashMoves paths_string
    in  result



path_construct = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'R','-'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]
path_construct2 = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'U','-'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]
path_construct3 = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'U','p'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]





squashDirections :: [(Char,Char)] -> [(Char,Char)]
squashDirections [] = []
squashDirections [x] = [x]
squashDirections (x1:x2:xs)
    | x1==x2 = squashDirections (x2:xs)
    | otherwise = x1:squashDirections (x2:xs)



reformatPath :: [(Integer,Integer,Char,Char)] -> [(Char,Char)]
reformatPath paths =  squashDirections $ [ (dir,item) | (_,_,dir,item) <- paths ]