import System.IO 
import Prelude
import Parser
import Data.Char 
import Data.List
import Control.Exception


{-- Section: Data types --}
type Item = Char
type Board = [[Item]]
type Direction = String
type Point = (Int,Int,Char)

data Move = Cond Item Direction | D Direction deriving (Eq)
data Command = Function | Loop Int Move Move | M Move deriving (Eq)

instance Show Move where
    show (D direction) = direction
    show (Cond cond direction) = "Cond{" ++ [cond] ++ "}" ++ "{"++ direction  ++"}" 

instance Show Command where
    show (Loop i m1 m2) = "Loop{" ++ show i ++ "}{" ++ show m1 ++ "," ++ show m2 ++ "}"
    show (Function) = "Function" 
    show (M m) = show m

data GameMap = GameMap 
               { getBoard :: Board ,
                 getHeight :: Int,
                 getWidth :: Int,
                 getCondPos :: [Point],
                 getPlayerPos :: (Int,Int), 
                 getTargetPos :: (Int,Int),
                 playerWon :: Bool
                }  


{-- ‘@’ represents the ball.
    ‘-’ represents a path block that the ball could roll on.
    ’*’ represents the grass (obstacles).
    ‘p’ represents the tile of the path’s block color is the special color : pink.
    ‘o’ represents the tile of the path’s block color is the special color : orange.
    ‘y’ represents the tile of the path’s block color is the special color : yellow.
    ‘b’ represents the bonus (the stars in Kodable).
    ‘t’ represents the target point --}
ball::Item
ball = '@'
path :: Item 
path = '-'
grass :: Item
grass = '*'
pink :: Item
pink = 'p'
orange :: Item 
orange = 'o'
yellow :: Item 
yellow = 'y' 
bonus :: Item
bonus = 'b'
target :: Item
target = 't'
conditionals :: [Item] 
conditionals = [orange,yellow,pink]





{-- Section : Helper utility functions --}

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

    
boardToStr :: Board -> String
boardToStr board = unlines (map (intersperse ' ') board)

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



validPosition :: (Int,Int) -> Board -> Bool 
validPosition (i,j) board = (i >= 0 && i < height) && (j >= 0 && j < width)
    where height = length board
          width = length (board !! 0)

{-- Section: Command Parsers--}
colorParser :: Parser Char
colorParser = char 'p' +++ char 'o' +++ char 'y'  

directionParser :: Parser Direction 
directionParser = string "Right" +++ string "Left" +++ string "Up" +++ string "Down" 

directionMoveParser :: Parser Move
directionMoveParser = do dir <-directionParser
                         return (D dir) 


numberParser :: Parser Char 
numberParser = char '1' +++ char '2' +++ char '3' +++ char '4' +++ char '5' 


commandParser :: Parser Command 
commandParser = functionParser +++ loopParser +++ moveParser

moveParser :: Parser Command
moveParser = do move <- (conditionalParser +++ directionMoveParser)
                return (M move) 

functionParser :: Parser Command 
functionParser = do x <- string "Function"
                    if x == "Function"
                        then return Function
                    else failure 

loopParser :: Parser Command
loopParser = do string "Loop"
                char '{'
                number <- numberParser 
                char '}'  
                char '{'
                direction1 <- directionMoveParser +++ conditionalParser
                char ','
                direction2 <- directionMoveParser +++ conditionalParser
                char '}'
                return (Loop (digitToInt number) direction1 direction2)

conditionalParser :: Parser Move
conditionalParser = do string "Cond"
                       char '{'
                       color <- colorParser
                       char '}' 
                       char '{'
                       direction <- directionParser
                       char '}'
                       return (Cond color direction)


emptyGameMap :: GameMap
emptyGameMap = GameMap {getBoard=[],
                        getHeight=0,
                        getCondPos=[],
                        getWidth=0,
                        getPlayerPos=(0,0),
                        getTargetPos=(0,0),
                        playerWon=False}

information :: IO ()
information = do putStrLn "-- Kodable Game Commands -----------------"
                 putStrLn "- load - load map from txt file          -"
                 putStrLn "- check - check is the map is solvable   -"
                 putStrLn "- solve - give a solution for the map    -"
                 putStrLn "- quit - quit the game                   -"
                 putStrLn "- play - interactive action from player  -"
                 putStrLn "------------------------------------------"

start_kodable :: IO () 
start_kodable = do information 
                   kodable emptyGameMap

solveIO :: GameMap -> IO () 
solveIO gamemap = if isBoardSolvable (getBoard gamemap)
                        then do putStrLn "A solution to this game:" 
                                putStrLn (intercalate " " $ optimalPath (getBoard gamemap))
                                kodable gamemap
                  else do putStrLn "This board cannot be solved, quitting game.."
                          return ()


checkIO :: GameMap -> IO ()
checkIO gamemap = if isBoardSolvable (getBoard gamemap) 
                     then do putStrLn "This board is solvable!"
                             kodable gamemap 
                  else do putStrLn "This board cannot be solved, quitting game.." 
                          return ()

quitIO :: IO () 
quitIO = do putStrLn "Quitting game.." 
            return () 


loadIO :: [String] -> IO ()
loadIO command_list = do game_map <- loadFile (command_list !! 1)
                         kodable game_map

kodable :: GameMap -> IO ()
kodable gamemap = do 
    putStr ">"
    command <- getLine
    let command_list = words command
    if head command_list == "quit"
        then quitIO
    else if (head command_list == "check")
        then checkIO gamemap
    else if (head command_list == "solve")
        then solveIO gamemap
    else if (head command_list == "load")
            then if (length command_list == 2)
                    then do loadIO command_list
            else do putStrLn "~ ~ Please input one filename as argument ~ ~"
                    kodable gamemap

    else if head command_list == "play"
            then playLoop gamemap

    else do kodable gamemap


isTargetReached :: GameMap -> Bool
isTargetReached gamemap = (target_pos==player_pos)
    where target_pos = getTargetPos gamemap 
          player_pos = getPlayerPos gamemap 

playLoop :: GameMap -> IO ()
playLoop gamemap = do direction_list <- play []
                      putStrLn (show direction_list)
                      new_gamemap <- (moveFullyList gamemap direction_list)
                      if isTargetReached new_gamemap 
                          then do putStrLn "You reached the target!"
                                  return () 
                      else playLoop new_gamemap

directions :: [String]
directions = ["Left","Right","Up","Down"]

refreshBoard :: Board -> [Point] -> Board 
refreshBoard board [] = board
refreshBoard board (p:ps) = refreshBoard new_board ps
    where (i,j,color) = p
          new_board = edit2DArray i j color board

refreshGameMap :: GameMap -> GameMap 
refreshGameMap game_map = GameMap { getBoard=refreshed_board,
                                    getHeight = height, 
                                    getWidth = width,
                                    getCondPos=cond_pos,
                                    getPlayerPos=player_pos,
                                    getTargetPos=target_pos,
                                    playerWon=has_won } 
    where board  = getBoard game_map 
          cond_pos = getCondPos game_map
          refreshed_board = refreshBoard board cond_pos 
          height = getHeight game_map 
          width = getWidth game_map 
          player_pos = getPlayerPos game_map 
          has_won = playerWon game_map 
          target_pos = getTargetPos game_map

getPlayDirection :: [Command] -> IO [Command]
getPlayDirection xs = do command <- getLine
                         let stripped_command = stripWhiteSpaces command -- take away trailing whitespaces
                             parsed_command = runParser commandParser stripped_command
                             parsed_command_type = fst $ head parsed_command
                         if (not $ null command) && (not $ null $ parsed_command) 
                            then do play (xs ++ [parsed_command_type])
                         else do return xs

-- invalid input will cause the play IO to return 
play :: [Command] -> IO [Command]
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
        rectangular = isRectangle cleanboard
        boardHeight = length cleanboard
        boardWidth = length (head cleanboard)
        target_pos = findTargetPos cleanboard
        cond_pos = findConditionalsPos cleanboard
        playerPos = findPlayerPos cleanboard
        new_map = GameMap {getBoard=cleanboard, 
                           getHeight=boardHeight, 
                           getWidth=boardWidth, 
                           getCondPos = cond_pos, 
                           getTargetPos = target_pos,
                           getPlayerPos = playerPos,
                           playerWon = False}
    -- check if map is rectangular
    if rectangular
        then do putStrLn "Read map successfully!"
                putStrLn "Initial:"
                putStrLn (boardToStr cleanboard)
                hClose handle  
                return new_map 
    else (do putStrLn "This map is not in the correct format"
             putStrLn "Map must be rectangular shaped"
             putStrLn "Please load another map to play"
             hClose handle  
             return emptyGameMap)





getItemFromPos :: Board -> Int -> Int -> Item 
getItemFromPos board i j = (board !! i) !! j


getItemFromPosTuple :: Board -> (Int,  Int) -> Item 
getItemFromPosTuple board (i,j) = if validPosition (i,j) board then (board !! i) !! j else 'X' -- X means invalid (out of bounds)




-- check if next item is a bonus 
testBonus :: GameMap -> Direction -> Bool  
testBonus game_map direction 
        | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) == bonus)
        | direction == "Right" = (j<width) && (getItemFromPos board i (j+1) == bonus)
        | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j == bonus) 
        | direction == "Down" = (i<height-1) && (getItemFromPos board (i+1) j == bonus )
        where (i,j) = getPlayerPos game_map 
              board = getBoard game_map
              width = getWidth game_map 
              height = getHeight game_map

-- check if item is traversable (not grass)
testDirection :: GameMap -> Direction -> Bool  
testDirection game_map direction 
        | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) /= grass)
        | direction == "Right" = (j<width-1) && (getItemFromPos board i (j+1) /= grass)
        | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j /= grass) 
        | direction == "Down" = (i<height) && (getItemFromPos board (i+1) j /= grass)
        where (i,j) = getPlayerPos game_map 
              board = getBoard game_map
              width = getWidth game_map 
              height = getHeight game_map

-- check if item is conditional specifically on the color 
testDirectionConditionalSpecific :: GameMap -> Item -> Direction -> Bool  
testDirectionConditionalSpecific game_map color direction 
        | direction == "Left" = (j >= 1) && (getItemFromPos board i (j-1) == color)
        | direction == "Right" = (j<width) && (getItemFromPos board i (j+1) == color )
        | direction == "Up" = (i>=1) && (getItemFromPos board (i-1) j == color ) 
        | direction == "Down" = (i<height) && (getItemFromPos board (i+1) j == color)
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
                                                  getCondPos = cond_pos,
                                                  getTargetPos = target_pos,         
                                                  getPlayerPos = (new_i,new_j),
                                                  playerWon = False
                                                 }
    where height = getHeight game_map
          width = getWidth game_map
          cond_pos = getCondPos game_map
          target_pos = getTargetPos game_map


commandIsDirection :: Command -> Bool 
commandIsDirection cmd = case cmd of (M (D direction)) -> True 
                                     _ -> False 

commandIsCond :: Command -> Bool 
commandIsCond cmd = case cmd of (M (Cond item dir)) -> True 
                                _ -> False

moveFullyList :: GameMap -> [Command] -> IO GameMap
moveFullyList game_map [] = return game_map     
moveFullyList game_map [(M (Cond _ _))] = return game_map       
moveFullyList game_map [cmd] = 
    let (M (D dir)) = cmd
        moveDir = moveOneStep game_map dir 
        ref_moveDir = refreshGameMap moveDir
        dirTested = testDirection game_map dir
        dirBonus = testBonus game_map dir
    in if dirTested && dirBonus 
        then do putStrLn "You got a bonus"
                new_gamemap <- (moveFullyList ref_moveDir [cmd])
                return new_gamemap
        else if dirTested
                then do new_gamemap <- (moveFullyList ref_moveDir [cmd])
                        return new_gamemap
        else do putStrLn (boardToStr $ getBoard game_map)
                return game_map

moveFullyList game_map (cmd1:cmd2:cmds) 
    | (commandIsDirection cmd1) && (commandIsDirection cmd2) =
        (if dir1Tested && dir1Bonus 
            then do putStrLn ("You got a bonus!")
                    new_game <- moveFullyList ref_moveDir1 full_commands
                    return new_game
        else if dir1Tested 
                then do new_game <- moveFullyList ref_moveDir1 full_commands
                        return (refreshGameMap new_game)
        else do putStrLn (boardToStr $ getBoard game_map)
                new_game <- moveFullyList moveDir2 new_commands
                return new_game)
    | (commandIsDirection cmd1) && (commandIsCond cmd2) = 
        if dir1TestConditional && dir2TestConditionalSpecific
            then do putStrLn (boardToStr $ getBoard game_map)
                    new_game <- moveFullyList moveDir1 cond_commands
                    return new_game
        else if dir1Tested && dir1Bonus 
                then do putStrLn ("You got a bonus!")
                        new_game <- moveFullyList ref_moveDir1 full_commands
                        return new_game 
        else if dir1Tested 
                then do new_game <- moveFullyList ref_moveDir1 full_commands
                        return new_game
        else do putStrLn (boardToStr $ getBoard game_map)
                return game_map
    where (M (D dir1)) = cmd1
          (M (D dir2)) = cmd2
          (M (Cond color direction3)) = cmd2
          dir1Tested = testDirection game_map dir1
          dir2Tested = testDirection game_map dir2 
          dir1TestConditional = testDirection game_map dir1 
          dir2TestConditionalSpecific = testDirectionConditionalSpecific game_map color dir1 
          moveDir1 = moveOneStep game_map dir1 
          moveDir2 = moveOneStep game_map dir2
          ref_moveDir1 = refreshGameMap moveDir1
          full_commands = (cmd1:cmd2:cmds)
          new_commands = (cmd2:cmds)
          cond_commands = ((M (D direction3)):cmds)
          dir1Bonus = testBonus game_map dir1
          dir2Bonus = testBonus game_map dir2  




                                             


-- moveFully :: GameMap -> Command -> GameMap
-- moveFully game_map cmd 
--     | testDirectionConditional game_map direction = moveOneStep game_map direction
--     | testDirection game_map direction =  moveFully (moveOneStep game_map direction) cmd 
--     | otherwise = game_map
--     where (M (D direction)) = cmd


-- TODO will delete
test = do xs <- loadFile "test.txt"
          directions <- play []
          res <- moveFullyList xs directions 
          putStrLn (show $ getPlayerPos res)
          putStrLn (boardToStr $ getBoard (refreshGameMap res))
          putStrLn (show $ getCondPos res)



-- -- if game across a condition then we do one more 
-- -- else if game come across bonus then we add the bonus to the gameMap (increment bonus point) 
-- -- else if game come across path then recursion 
-- -- else just return the game
-- -- dont stop when meeting conditional unless the direction is conditional. 
-- moveFully :: GameMap -> Direction -> GameMap
-- moveFully game_map direction =
--     if testDirectionConditional game_map direction
--         then moveOneStep game_map direction
--     else if testDirection game_map direction
--         then moveFully (moveOneStep game_map direction) direction
--     else game_map

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
                "*****-------****---b----*****",
                "*****************************",
                "*****************************"]


sample_board3 :: Board
sample_board3= ["*****-------------------*****",
                "*****b-----------------b*****",
                "*****-*****************-*****",
                "*****-**----*****----**-*****",
                "*****-**-yy-*****-yy-**-*****",
                "*****-**----*****----**-*****",
                "*****-******--*b-******p----*",
                "*****-******-***-******-***-*",
                "@-----******-***-******-----t",
                "*****-******-***-******-*****",
                "*****--------***--------*****",
                "*****************************",
                "*****************************"]

map2_2 = ["*****-------------------*****",
          "*****------------------b*****",
          "*****-*****************-*****",
          "*****-**----*****----**-*****",
          "*****-**----*****----**-*****",
          "*****-**----*****----**-*****",
          "*****-******--b-*******-*****",
          "*****-******-***-******-*****",
          "@----p******-***-******p----t",
          "*****-******-***-******-*****",
          "*****---b----***--------*****",
          "*****************************",
          "*****************************"]






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
isBoardSolvable [] = False
isBoardSolvable board =
    let (i,j) = findPlayerPos board 
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A')  
    in  bfsTraverse [start_pos_direction] end_pos [] board




isTraversable :: (Int,Int,Char) -> Board -> Bool
isTraversable (i,j,_)  board = isValidPos && (itemAtPos /= grass)
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 

-- Search all possible paths to complete game
searchAllPath :: Board -> (Int,Int,Char,Char) -> [(Int,Int,Char,Char)] -> [(Int,Int)] -> Bool -> [[(Int,Int,Char,Char)]] 
searchAllPath board (i,j,dir,item) paths visited bonusFound 
    | ((i,j) `elem` visited) = if bonusFound then concat [ searchAllPath board next new_path updated_visited False | next <- tp_item ] else [] 
    | (item == target) =  [new_path]
    | (item == bonus) = concat [ searchAllPath new_board next new_path [] bonusF | next <- tp_item ]
    | otherwise = concat [ searchAllPath board next new_path updated_visited bonusF | next <- tp_item ]
    where neighbors = getNeighbors (i,j,dir) board
          new_board = edit2DArray i j '-' board
          traversable_positions = filter (\pos -> isTraversable pos board) neighbors
          tp_item = [ (ii,jj,dd, getItemFromPosTuple board (ii,jj) ) | (ii,jj,dd) <- traversable_positions ]
          new_path = paths ++ [(i,j,dir, item)]
          updated_visited = visited ++ [(i,j)]
          bonusF = bonusFound || (item == bonus)




withBonus :: [(Int,Int)] -> Int -> [[(Int,Int,Char,Char)]] ->  [[(Int,Int,Char,Char)]]
withBonus bonuses n [] = [] 
withBonus bonuses n (p:paths) = 
    let path_coord = [ (i,j) | (i,j,_,_) <- p]
        has_N_Bonus = (length (intersect bonuses path_coord) == n)
        rest_path = withBonus bonuses n paths
    in if has_N_Bonus then [p] ++ rest_path else [] ++ rest_path

pathWithThreeBonus :: Board -> [[(Int,Int,Char,Char)]]
pathWithThreeBonus board = 
    let bonuses = findBonusPos board 
        (i,j) = findPlayerPos board
        all_paths = searchAllPath board (i,j,'A',getItemFromPosTuple board (i,j)) [] [] False
    in withBonus bonuses 3 all_paths


shortestElementFromList :: Foldable t => [t a] -> t a
shortestElementFromList paths = head (sortOn length paths) 

optimalPath :: Board -> [String]
optimalPath board  
    | length three_bonus > 0 = shortestElementFromList (map reformatPath three_bonus)
    | length two_bonus > 0 = shortestElementFromList (map reformatPath two_bonus) 
    | length one_bonus > 0 = shortestElementFromList (map reformatPath one_bonus)
    | length no_bonus > 0 =  shortestElementFromList (map reformatPath no_bonus)
    where bonuses = findBonusPos board
          (i,j) = findPlayerPos board
          all_paths = searchAllPath board (i,j,'A',getItemFromPosTuple board (i,j)) [] [] False
          three_bonus = withBonus bonuses 3 all_paths
          two_bonus = withBonus bonuses 2 all_paths
          one_bonus = withBonus bonuses 1 all_paths 
          no_bonus = withBonus bonuses 0 all_paths




testSearchAllPath :: Board -> [String]
testSearchAllPath board =
    let (i,j) = findPlayerPos board 
        bonusPositions = findBonusPos board
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A', getItemFromPosTuple board (i,j))
        paths = searchAllPath board start_pos_direction [] [] False
        paths_string = [ [ (dir,item) | (x,y,dir,item) <- path] | path <-paths]
        result = map dirStrToCommands (map squashDirections paths_string)
    in  result



path_construct = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'R','-'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]
path_construct2 = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'U','-'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]
path_construct3 = [(8,0,'A','@'),(8,1,'R','-'),(8,2,'R','-'),(8,3,'R','-'),(8,4,'R','-'),(8,5,'R','-'),(9,5,'D','-'),(10,5,'D','-'),(10,6,'R','-'),(10,7,'R','-'),(10,8,'R','-'),(10,9,'R','-'),(10,10,'R','-'),(10,11,'R','-'),(10,12,'R','-'),(9,12,'U','-'),(8,12,'U','-'),(7,12,'U','-'),(6,12,'U','-'),(6,13,'R','-'),(6,14,'R','b'),(6,15,'R','-'),(6,16,'R','-'),(7,16,'D','-'),(8,16,'D','-'),(9,16,'D','-'),(10,16,'D','-'),(10,17,'R','-'),(10,18,'R','-'),(10,19,'R','-'),(10,20,'R','-'),(10,21,'R','-'),(10,22,'R','-'),(10,23,'R','-'),(9,23,'U','-'),(8,23,'U','p'),(8,24,'U','p'),(8,25,'R','-'),(8,26,'R','-'),(8,27,'R','-'),(8,28,'R','t')]

path_construct4 = [(2,2,'R','-'), (2,2,'R','p'),  (2,2,'D','-')  , (2,2,'D','y') , (2,2,'D','y')    ]



squashDirections :: [(Char,Char)] -> [Char]
squashDirections [] = []
squashDirections [(d,i)] = [d]
squashDirections (x1:x2:xs)
    | dir1==dir2 = squashDirections (x2:xs)
    | item1 `elem` conditionals = [dir1] ++ [item1] ++ squashDirections (x2:xs)
    | otherwise = dir1:squashDirections (x2:xs)
    where (dir1,item1) = x1
          (dir2,item2) = x2 


charToCommandMapping :: Char -> String 
charToCommandMapping chr 
    | chr == 'R' = "Right"
    | chr == 'L' =  "Left"
    | chr == 'U' = "Up"
    | chr == 'D' = "Down"
    | chr == 'p' = "Cond{p}{"
    | chr == 'o' = "Cond{o}{"
    | chr == 'y' = "Cond{y}{"
    | otherwise = ""

dirStrToCommands :: String -> String
dirStrToCommands [] = []
dirStrToCommands [x] = charToCommandMapping x 
dirStrToCommands (x:y:str) 
    | x `elem` conditionals = charToCommandMapping x ++ charToCommandMapping y ++ "}" ++ " " ++ dirStrToCommands str 
    | otherwise = charToCommandMapping x ++ " " ++ dirStrToCommands (y:str)




reformatPath :: [(Int,Int,Char,Char)] -> [String]
reformatPath paths =  words (dirStrToCommands (squashDirections $ [ (dir,item) | (_,_,dir,item) <- paths ]))


printOutList :: [String] -> IO ()
printOutList list = do putStrLn (unlines list)