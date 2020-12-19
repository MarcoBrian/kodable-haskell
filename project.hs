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
data Command = Function | Loop Int Move Move | M Move | Help deriving (Eq)

instance Show Move where
    show (D direction) = direction
    show (Cond cond direction) = "Cond{" ++ [cond] ++ "}" ++ "{"++ direction  ++"}" 

instance Show Command where
    show (Loop i m1 m2) = "Loop{" ++ show i ++ "}{" ++ show m1 ++ "," ++ show m2 ++ "}"
    show (Function) = "Function" 
    show (M m) = show m
    show (Help) = "Help"

data GameMap = GameMap 
               { getBoard :: Board ,
                 getHeight :: Int,
                 getWidth :: Int,
                 getCondPos :: [Point],
                 getFunction :: [Command],
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

directions :: [Direction]
directions = ["Left","Right","Up","Down"]

-- Initialize empty game map
emptyGameMap :: GameMap
emptyGameMap = GameMap {getBoard=[],
                        getHeight=0,
                        getCondPos=[],
                        getWidth=0,
                        getFunction = [],
                        getPlayerPos=(0,0),
                        getTargetPos=(0,0),
                        playerWon=False}

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

getItemFromPos :: Board -> Int -> Int -> Item 
getItemFromPos board i j = (board !! i) !! j

getItemFromPosTuple :: Board -> (Int,  Int) -> Item 
getItemFromPosTuple board (i,j) = if validPosition (i,j) board then (board !! i) !! j else 'X' -- X means invalid (out of bounds)

itemIsConditional :: Item -> Bool 
itemIsConditional item = item `elem` conditionals 

itemIsGrass :: Item -> Bool 
itemIsGrass item = item == grass 

commandIsDirection :: Command -> Bool 
commandIsDirection cmd = case cmd of (M (D direction)) -> True 
                                     _ -> False 

commandIsCond :: Command -> Bool 
commandIsCond cmd = case cmd of (M (Cond item dir)) -> True 
                                _ -> False


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

helpParser :: Parser Command
helpParser = do s <- (string "Help" +++ string "help")
                return (Help)

commandParser :: Parser Command 
commandParser = functionParser +++ loopParser +++ moveParser +++ helpParser

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


{-- Section IO --}
informationIO :: IO ()
informationIO = do putStrLn "-- Kodable Game Commands ------------------"
                   putStrLn "- load - load map from txt file          --"
                   putStrLn "- check - check is the map is solvable   --"
                   putStrLn "- solve - give a solution for the map    --"
                   putStrLn "- quit - quit the game                   --"
                   putStrLn "- play - interactive action from player  --"
                   putStrLn "-                                         -"
                   putStrLn "~ Please load a valid map before starting ~"
                   putStrLn "-------------------------------------------"

solveIO :: GameMap -> IO () 
solveIO gamemap = if isBoardSolvable (getBoard gamemap)
                        then do putStrLn "A solution to this game:" 
                                let optimal_path = optimalPath (getBoard gamemap)
                                    with_FunctionLoops = optimizedFunctionLoops optimal_path
                                    solution_path = fst with_FunctionLoops
                                    function_def = snd with_FunctionLoops
                                    str_path = intercalate " " solution_path
                                    str_func = intercalate " " function_def 
                                if not $ null function_def 
                                    then do putStrLn (str_path ++ " with " ++ str_func)
                                            kodable gamemap 
                                else do putStrLn str_path
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
                           getFunction = [],
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

main :: IO () 
main = do informationIO 
          kodable emptyGameMap

kodable :: GameMap -> IO ()
kodable gamemap = do 
    putStr ">"
    command <- getLine
    let command_list = words command
    if null command_list -- empty input restart cursor 
        then kodable gamemap
    else if head command_list == "quit"
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
            then if isBoardSolvable (getBoard gamemap)
                    then if (length command_list == 4) 
                            then do let functions = (convertStringsToCommands $ tail command_list)
                                        in if length functions == 3 
                                                then playLoop (initFunctionInGameMap gamemap functions)
                                           else playLoop gamemap
                         else playLoop gamemap
                 else do putStrLn "~ ~ Please load a solvable map first ~ ~"
                         kodable gamemap
    else do kodable gamemap


initFunctionInGameMap :: GameMap -> [Command] -> GameMap 
initFunctionInGameMap game_map function_def = GameMap {getBoard = board, 
                                                    getHeight= height,
                                                    getWidth = width,   
                                                    getCondPos = cond_pos,
                                                    getFunction = function_def,
                                                    getTargetPos = target_pos,         
                                                    getPlayerPos = playerPos,
                                                    playerWon = False
                                                 }
        where board = getBoard game_map
              height = getHeight game_map
              width = getWidth game_map
              cond_pos = getCondPos game_map
              target_pos = getTargetPos game_map
              playerPos = getPlayerPos game_map 



convertStringsToCommands :: [String] -> [Command]
convertStringsToCommands [] = []
convertStringsToCommands (x:xs) = [(fst $ head $ runParser moveParser x)] ++ convertStringsToCommands xs

isTargetReached :: GameMap -> Bool
isTargetReached gamemap = (target_pos==player_pos)
    where target_pos = getTargetPos gamemap 
          player_pos = getPlayerPos gamemap 

playLoop :: GameMap -> IO ()
playLoop gamemap = do direction_list_raw <- play []
                      let function_def = getFunction gamemap 
                          direction_list = convertCommandToBasicMove direction_list_raw function_def                       
                      if Help `elem` direction_list
                          then do let optimal = take 3 (optimalPath (getBoard gamemap))
                                  putStrLn ("Try these: " ++ (show optimal) )
                                  playLoop gamemap 
                      else do new_gamemap <- (moveFullyList gamemap direction_list True)
                              if isTargetReached new_gamemap 
                                then do putStrLn "Congratulations! You won the game!"
                                        return () 
                              else playLoop new_gamemap

-- invalid input will cause the play IO to return 
play :: [Command] -> IO [Command]
play xs = do if null xs
                then do putStr "First   Direction : "
                        getPlayDirection xs
             else do putStr "Next    Direction : "
                     getPlayDirection xs

getPlayDirection :: [Command] -> IO [Command]
getPlayDirection xs = do command <- getLine
                         let stripped_command = stripWhiteSpaces command -- take away trailing whitespaces
                             parsed_command = runParser commandParser stripped_command
                             parsed_command_type = fst $ head parsed_command
                         if ((not $ null command) && (not $ null $ parsed_command)) 
                            then do if (parsed_command_type `elem` [Help]) == True
                                        then return [Help]
                                    else play (xs ++ [parsed_command_type])
                         else do return xs



convertCommandToBasicMove :: [Command] -> [Command] -> [Command]
convertCommandToBasicMove [] function_def = []
convertCommandToBasicMove (cmd:cmds) function_def = 
    case cmd of (Loop int move1 move2) -> (loopToBasicMoveConverter cmd) ++ convertCommandToBasicMove cmds function_def
                (Function) -> function_def ++ convertCommandToBasicMove cmds function_def
                _ -> [cmd] ++ convertCommandToBasicMove cmds function_def

loopToBasicMoveConverter :: Command -> [Command]
loopToBasicMoveConverter loop = concat $ replicate count [new_move1,new_move2] 
    where (Loop count move1 move2) = loop
          new_move1 = M move1 
          new_move2 = M move2 


getGameFunction :: GameMap -> [Command]
getGameFunction gamemap = getFunction gamemap 



refreshGameMap :: GameMap -> GameMap 
refreshGameMap game_map = GameMap { getBoard=refreshed_board2,
                                    getHeight = height, 
                                    getWidth = width,
                                    getCondPos= cond_pos,
                                    getFunction = function_def ,
                                    getPlayerPos=(i,j),
                                    getTargetPos=target_pos,
                                    playerWon=has_won } 
    where board  = getBoard game_map 
          cond_pos = getCondPos game_map
          refreshed_board = refreshBoard board cond_pos 
          refreshed_board2 = edit2DArray i j ball refreshed_board
          height = getHeight game_map 
          width = getWidth game_map 
          (i,j) = getPlayerPos game_map 
          has_won = playerWon game_map 
          function_def = getFunction game_map 
          target_pos = getTargetPos game_map

refreshBoard :: Board -> [Point] -> Board 
refreshBoard board [] = board
refreshBoard board (p:ps) = refreshBoard new_board ps
    where (i,j,color) = p
          new_board = edit2DArray i j color board

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
                                                  getFunction = function_def,
                                                  getTargetPos = target_pos,         
                                                  getPlayerPos = (new_i,new_j),
                                                  playerWon = False
                                                 }
    where height = getHeight game_map
          width = getWidth game_map
          cond_pos = getCondPos game_map
          function_def = getFunction game_map
          target_pos = getTargetPos game_map




{-- Section interactive movement logic --}
moveFullyList :: GameMap -> [Command] -> Bool -> IO GameMap
moveFullyList game_map [] isStartingMove = return game_map     
moveFullyList game_map [(M (Cond _ _))] isStartingMove = return game_map       
moveFullyList game_map [cmd] isStartingMove = 
    let (M (D dir)) = cmd
        moveDir = moveOneStep game_map dir 
        ref_moveDir = refreshGameMap moveDir
        dirTested = testDirection game_map dir
        dirBonus = testBonus game_map dir
        not_starting_move = False
    in if dirTested && dirBonus 
        then do putStrLn "You got a bonus!"
                new_gamemap <- (moveFullyList ref_moveDir [cmd] not_starting_move) 
                return new_gamemap
        else if dirTested
                then do new_gamemap <- (moveFullyList ref_moveDir [cmd] not_starting_move) 
                        return new_gamemap
        else if isStartingMove
                then do putStrLn ("Sorry cannot move " ++ dir)
                        return game_map
        else do putStrLn (boardToStr $ getBoard game_map)
                return game_map

moveFullyList game_map (cmd1:cmd2:cmds) isStartingMove
    | (commandIsDirection cmd1) && (commandIsDirection cmd2) =
        (if dir1Tested && dir1Bonus 
            then do putStrLn ("You got a bonus!")
                    new_game <- moveFullyList ref_moveDir1 full_commands False 
                    return new_game
        else if dir1Tested 
                then do new_game <- moveFullyList ref_moveDir1 full_commands False 
                        return (refreshGameMap new_game)
        else if isStartingMove
                then do putStrLn ("Sorry cannot move " ++ dir1)
                        return game_map
        else do putStrLn (boardToStr $ getBoard game_map)
                new_game <- moveFullyList moveDir2 new_commands True 
                return new_game)
    | (commandIsDirection cmd1) && (commandIsCond cmd2) = 
        if dir1TestConditional && dir2TestConditionalSpecific
            then do putStrLn (boardToStr $ getBoard game_map)
                    new_game <- moveFullyList moveDir1 cond_commands True
                    return new_game
        else if dir1Tested && dir1Bonus 
                then do putStrLn ("You got a bonus!")
                        new_game <- moveFullyList ref_moveDir1 full_commands False
                        return new_game 
        else if dir1Tested 
                then do new_game <- moveFullyList ref_moveDir1 full_commands False
                        return new_game
        else if isStartingMove
                then do putStrLn ("Sorry cannot move "++dir1)
                        return game_map
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

-- Lowest level matrix editing for the UI --
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
 

{-- Section: Check board solvability and finding solution --}

traversablePosition :: (Int,Int,Char) -> [(Int,Int)] -> Board -> Bool
traversablePosition (i,j,_) visited board = isValidPos && (itemAtPos /= grass) && (not isVisited)
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 
          isVisited = (i,j) `elem` visited

isTraversable :: (Int,Int,Char) -> Board -> Bool
isTraversable (i,j,_)  board = isValidPos && (itemAtPos /= grass)
    where isValidPos = validPosition (i,j) board
          itemAtPos = getItemFromPos board i j 

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
          

-- Bread First Search Approach (used to check solvability) 
bfsTraverse :: [(Int,Int,Char)] -> (Int,Int) -> [(Int,Int)] -> Board -> Bool
bfsTraverse [] _ _ _ = False
bfsTraverse ((i, j, direction):xs) end visited board  
    | ((i,j) /= end) = bfsTraverse new_queue end updated_visited board
    | otherwise = True 
   where neighbors = getNeighbors (i,j,direction) board -- LEFT, RIGHT , UP, DOWN 
         traversable_positions = filter (\pos -> traversablePosition pos visited board) neighbors
         updated_visited = visited ++ [(i,j)]
         new_queue = xs ++ traversable_positions

-- Used to construct a path of coordinates  
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

isBoardSolvable :: Board -> Bool 
isBoardSolvable [] = False
isBoardSolvable board =
    let (i,j) = findPlayerPos board 
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A')  
    in  bfsTraverse [start_pos_direction] end_pos [] board

-- driver code -- 
pathToEnd :: Board -> [(Int,Int,Char,Char)]
pathToEnd board = 
    let (i,j) = findPlayerPos board 
        bonusPositions = findBonusPos board
        end_pos = findTargetPos board 
        start_pos_direction = (i,j, 'A', getItemFromPosTuple board (i,j) )
    in  reverse (bfsTraversePath [start_pos_direction] end_pos [] [] board)



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


-- Find paths with n bonuses --
withBonus :: [(Int,Int)] -> Int -> [[(Int,Int,Char,Char)]] ->  [[(Int,Int,Char,Char)]]
withBonus bonuses n [] = [] 
withBonus bonuses n (p:paths) = 
    let path_coord = [ (i,j) | (i,j,_,_) <- p]
        has_N_Bonus = (length (intersect bonuses path_coord) == n)
        rest_path = withBonus bonuses n paths
    in if has_N_Bonus then [p] ++ rest_path else [] ++ rest_path

-- driver test code -- 
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

-- test driver code --
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

{-- Section : Path Coordinates to String Directions converter --}

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
    | chr == 'L' = "Left"
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

{--Section to convert paths to contain loops and functions --}

generateTwoMoveCombinations :: [String] -> [[String]]
generateTwoMoveCombinations [] = [] 
generateTwoMoveCombinations [x] = [] 
generateTwoMoveCombinations (x:y:xs) = [[x]++[y]] ++ generateTwoMoveCombinations (y:xs)  

generateUniqueTwoMoveComb :: [String] -> [[String]]
generateUniqueTwoMoveComb commands = nub (generateTwoMoveCombinations commands)

transformLoopAux :: [[String]] -> [String] -> [String]
transformLoopAux [] commands = commands 
transformLoopAux (comb:combs) commands = transformLoopAux combs looped
    where looped = basicCommandToLoopConverter commands comb 0 

transformLoop :: [String] -> [String]
transformLoop commands = transformLoopAux uniqueTwoCombs commands
    where uniqueTwoCombs = generateUniqueTwoMoveComb commands 

basicCommandToLoopConverter :: [String] -> [String] -> Int -> [String]
basicCommandToLoopConverter [] baseMoves loopCounter = 
    let move1 = baseMoves !! 0 
        move2 = baseMoves !! 1
        loopString = "Loop{" ++ (show loopCounter) ++ "}{" ++ move1 ++ ","  ++ move2 ++  "}"
    in if loopCounter > 1 
          then [loopString]
       else if loopCounter == 1
               then baseMoves 
       else []
basicCommandToLoopConverter [x1] baseMoves loopCounter = 
    let  move1 = baseMoves !! 0 
         move2 = baseMoves !! 1
         loopString = "Loop{" ++ (show loopCounter) ++ "}{" ++ move1 ++ ","  ++ move2 ++  "}"
    in if loopCounter == 0 
          then [x1] 
       else if loopCounter == 1 
          then baseMoves ++ [x1]
       else [loopString] ++ [x1] 
basicCommandToLoopConverter (x1:x2:cmds) baseMoves loopCounter 
    | (x1 == move1 && x2 == move2) = basicCommandToLoopConverter cmds baseMoves (loopCounter + 1)
    | otherwise = if loopCounter > 1 
                     then [loopString] ++ (basicCommandToLoopConverter (x1:x2:cmds) baseMoves 0)
                  else if loopCounter == 1
                          then baseMoves ++ (basicCommandToLoopConverter (x1:x2:cmds) baseMoves 0)
                  else [x1] ++ (basicCommandToLoopConverter (x2:cmds) baseMoves 0)
    where move1 = baseMoves !! 0 
          move2 = baseMoves !! 1
          loopString = "Loop{" ++ (show loopCounter) ++ "}{" ++ move1 ++ ","  ++ move2 ++  "}"

generateAllFunctions :: [[String]]
generateAllFunctions = [ [d,d2,d3] | d <- directions , d2 <- directions , d3 <- directions, d /= d2 && d3 /= d2 ] ++ [[]]

transformFunctionAux :: [String] -> [String] -> [String]
transformFunctionAux [] function_def = [] 
transformFunctionAux commands function_def 
    | isFunction = ["Function"] ++ (transformFunctionAux rest_of_commands function_def )
    | otherwise = [head_command] ++ (transformFunctionAux tail_command function_def)
    where isFunction = (take 3 commands) == function_def
          rest_of_commands = (drop 3 commands)
          head_command =  head commands
          tail_command = tail commands
          
transformFunction :: [String] -> [String] -> ([String],[String])
transformFunction commands function_def = (transformed,function_def)
    where transformed = transformFunctionAux commands function_def 

generateAllTransformFunction :: [String] -> [([String],[String])]
generateAllTransformFunction commands = map (\function_def -> transformFunction commands function_def ) all_functions
    where all_functions = generateAllFunctions


sortByLength :: (Foldable t1, Foldable t2, Foldable t3, Foldable t4) => (t1 a1, t3 a2) -> (t2 a3, t4 a4) -> Ordering
sortByLength (a1, b1) (a2, b2)
  | length a1 < length a2 = LT
  | length a1 > length a2 = GT
  | length a1 == length a2 = compare (length b1) (length b2)

transformLoopThenFunction :: [String] -> ([String],[String]) 
transformLoopThenFunction commands = head_command 
   where loop_transformed = transformLoop commands
         function_transformed_all = generateAllTransformFunction loop_transformed
         sorted = sortBy sortByLength function_transformed_all
         head_command = head sorted

transformFunctionThenLoop :: [String] -> ([String],[String])
transformFunctionThenLoop commands = (final,function_def)
    where unique_two_combs = generateUniqueTwoMoveComb commands
          function_transformed_all = generateAllTransformFunction commands
          sorted = sortBy sortByLength function_transformed_all
          head_sorted = head sorted 
          head_sorted_list = fst head_sorted
          function_def = snd head_sorted 
          final = transformLoopAux unique_two_combs head_sorted_list

optimizedFunctionLoops :: [String] -> ([String],[String])
optimizedFunctionLoops commands = head_sorted
    where function_first = transformFunctionThenLoop commands
          loop_first = transformLoopThenFunction commands
          head_sorted = head $ sortBy sortByLength $ [function_first] ++ [loop_first]

t :: [String]
t = ["Cond{p}{Right}","Up","Cond{p}{Right}","Up","Right","Up","Right","Up","Right"]

s :: [String]
s = ["Right","Up","Right"]

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