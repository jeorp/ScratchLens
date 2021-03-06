{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lens -- import from src/Lens.hs

import System.Exit
import Control.Monad.State
import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Arrow

import System.Random
import Data.Proxy
import GHC.TypeLits
import Data.Default.Class



-- example program 

data Point = 
  Point 
  {
    _x :: Int, 
    _y :: Int
  } deriving (Show, Eq)


x :: Lens' Point Int
x = lens (\(Point x _) -> x) (\p x -> p {_x=x})

y :: Lens' Point Int
y = lens (\(Point _ y) -> y) (\p y -> p {_y=y})

instance Num Point where
  (Point px py) + (Point qx qy) = Point (px+qx) (py+qy)
  (Point px py) * (Point qx qy) = Point (px*qx) (py*qy)
  abs (Point px py) = Point (abs px) (abs py)
  signum (Point px py) = Point (signum px) (signum py)
  fromInteger i = Point (fromInteger i) (fromInteger i)
  negate (Point px py) = Point (negate px) (negate py)

dist :: Point -> Point -> Int
dist p q = let r = abs (p - q) in r ^. x + r ^. y

vectorToDir :: Point -> Direction
vectorToDir p = 
  let absP = abs p
      absX = absP ^. x
      absY = absP ^. y
  in if absX > absY
      then if p ^. x >= 0
        then RIGHT
        else LEFT
      else if p ^. y >= 0
        then UP
        else DOWN 

data Extra = 
  Extra 
  {
    _moveTwo :: Bool, 
    _trans :: Bool, 
    _flash :: Bool
  } deriving (Show, Eq)

moveTwo :: Lens' Extra Bool
moveTwo = lens (\(Extra b _ _) -> b) (\e b -> e {_moveTwo=b})

trans :: Lens' Extra Bool
trans = lens (\(Extra _ b _) -> b) (\e b -> e {_trans=b})

flash :: Lens' Extra Bool
flash = lens (\(Extra _ _ b) -> b) (\e b -> e {_flash=b})

data Ex = MoveTwo | Trans | Flash deriving (Eq, Ord, Enum, Show, Bounded)

instance Default Ex where
  def = MoveTwo

extraToEx :: Extra -> [Ex]
extraToEx ex = 
  let xs = []
      addMoveTwo = if ex ^. moveTwo then MoveTwo : xs else xs
      addTrans = if ex ^. trans then Trans : addMoveTwo else addMoveTwo
      addFlash = if ex ^. flash then Flash : addTrans else addTrans
    in addFlash

exToExtraLens :: Ex -> Lens' Extra Bool
exToExtraLens MoveTwo = moveTwo
exToExtraLens Trans = trans
exToExtraLens Flash = flash

moveTwo_ :: Lens' Ex Ex
moveTwo_ = lens (const MoveTwo) (const id)

trans_ :: Lens' Ex Ex
trans_ = lens (const Trans) (const id)

flash_ :: Lens' Ex Ex
flash_ = lens (const Flash) (const id)

type Name = String

data Player =
  Player 
  {
    _playerName :: Name, 
    _playerPoint :: Point,
    _extraFlag :: Bool,
    _extra :: Extra
  } deriving (Show, Eq)

playerName :: Lens' Player Name
playerName = lens (\(Player n _ _ _) -> n) (\obj n -> obj {_playerName=n})

playerPoint :: Lens' Player Point
playerPoint = lens (\(Player _ p _ _) -> p) (\obj p -> obj {_playerPoint=p})

extraFlag :: Lens' Player Bool
extraFlag = lens (\(Player _ _ e _) -> e) (\obj e -> obj {_extraFlag=e})

extra :: Lens' Player Extra
extra = lens (\(Player _ _ _ e) -> e) (\obj e -> obj {_extra=e})

data Dictionary = 
  Dictionary 
  {
    _word :: Name,
    _meaning :: String
  } deriving (Show, Eq)

word :: Lens' Dictionary Name
word = lens (\(Dictionary w _) -> w) (\d w -> d {_word=w})

meanig :: Lens' Dictionary String
meanig = lens (\(Dictionary _ m) -> m) (\d m -> d {_meaning=m})

data Enemy = 
  Enemy 
  {
    _enemyName :: Dictionary, 
    _enemyPoint :: Point, 
    _dis :: Int,
    _goal :: Bool,
    _hp :: Int
  } deriving (Show, Eq)

enemyName :: Lens' Enemy Dictionary
enemyName = lens (\(Enemy n _ _ _ _) -> n) (\e n -> e {_enemyName = n})

enemyPoint :: Lens' Enemy Point
enemyPoint = lens (\(Enemy _ p _ _ _) -> p) (\e p -> e {_enemyPoint = p})

dis :: Lens' Enemy Int
dis = lens (\(Enemy _ _ r _ _) -> r) (\e r -> e {_dis = r})

goal :: Lens' Enemy Bool
goal = lens (\(Enemy _ _ _ g _) -> g) (\e g -> e {_goal = g})

hp :: Lens' Enemy Int
hp = lens (\(Enemy _ _ _ _ h) -> h) (\e h -> e {_hp = h})

data World = 
  World 
  {
    _turn :: Int,
    _warpPoint :: Point, 
    _player :: Player, 
    _enemy :: Enemy,
    _logs :: [String]
  } deriving (Show, Eq)

turn :: Lens' World Int
turn = lens (\(World t _ _ _ _) -> t) (\w t -> w {_turn=t})

warpPoint :: Lens' World Point
warpPoint = lens (\(World _ p _ _ _) -> p) (\w p -> w {_warpPoint=p})

player :: Lens' World Player
player = lens (\(World _ _ p _ _) -> p) (\w p -> w {_player=p})

enemy :: Lens' World Enemy
enemy = lens (\(World _ _ _ e _) -> e) (\w e -> w {_enemy=e})

logs :: Lens' World [String]
logs = lens (\(World _ _ _ _ l) -> l) (\w l -> w {_logs=l})

class HasPoint c where
  pos :: Lens' c  Point

instance HasPoint Player where
  pos = playerPoint

instance HasPoint Enemy where
  pos = enemyPoint

class HasName c where
   name :: Lens' c Name

instance HasName Player where
  name = playerName

instance HasName Enemy where
  name = enemyName . word

type Game = ReaderT Config  (StateT World IO)

type Size = Int

data Direction = RIGHT | LEFT | UP | DOWN deriving (Eq, Ord, Enum, Show, Bounded)

instance Default Direction where
  def = RIGHT

right_ :: Lens' Direction Direction 
right_ = lens (const RIGHT) (\dir s -> s)

left_ :: Lens' Direction Direction 
left_ = lens (const LEFT) (\dir s -> s)

down_ :: Lens' Direction Direction 
down_ = lens (const DOWN) (\dir s -> s)

up_ :: Lens' Direction Direction 
up_ = lens (const UP) (\dir s -> s)

plusOne :: Int -> Int
plusOne i = i + 1

minusOne :: Int -> Int
minusOne i = i - 1

dirToFunctional :: Direction -> Point -> Point
dirToFunctional LEFT = over minusOne x
dirToFunctional RIGHT = over plusOne x
dirToFunctional UP = over plusOne y
dirToFunctional DOWN = over minusOne y

executable :: Size -> Point -> [Direction]
executable s p = 
  let xs = []
      addLeft = if p ^. x > 0 then LEFT : xs else xs
      addRight = if p ^. x < s then RIGHT : addLeft else addLeft
      addDown = if p ^. y > 0 then DOWN : addRight else addRight
      addUp = if p ^. y < s then UP : addDown else addDown
      in addUp

data Answer = Yes | No deriving (Eq, Ord, Enum, Show, Bounded)

instance Default Answer where
  def = Yes

yes_ :: Lens' Answer Answer 
yes_ = lens (const Yes) (\ans s -> s)

no_ :: Lens' Answer Answer 
no_ = lens (const No) (\ans s -> s)

data RPS = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show, Bounded)

instance Default RPS where
  def = Rock

rock_ :: Lens' RPS RPS 
rock_ = lens (const Rock) (\rps s -> s)

paper_ :: Lens' RPS RPS 
paper_ = lens (const Paper) (\rps s -> s)

scissors_ :: Lens' RPS RPS 
scissors_ = lens (const Scissors) (\rps s -> s)

data RPSResult = Win | Defeat | Draw deriving (Show, Eq)

rpsGo :: RPS -> RPS -> RPSResult
rpsGo Rock j = case j of
  Rock -> Draw
  Paper -> Defeat
  Scissors -> Win
rpsGo Paper j = case j of
  Rock -> Win
  Paper -> Draw
  Scissors -> Defeat
rpsGo Scissors j = case j of
  Rock -> Defeat
  Paper -> Win
  Scissors -> Draw


class Registered l where
  command :: Proxy l -> String

instance Registered "y" where
  command _ = "Yes"

instance Registered "n" where
  command _ = "No"

instance Registered "l" where
  command _ = "Left"

instance Registered "r" where
  command _ = "Right"

instance Registered "d" where
  command _ = "Down"

instance Registered "u" where
  command _ = "Up"

instance Registered "w" where
  command _ = "Move Two Step"

instance Registered "t" where
  command _ = "Transport"

instance Registered "f" where
  command _ = "Flash"

instance Registered "rock" where
  command _ = "Rock"

instance Registered "paper" where
  command _ = "Paper"

instance Registered "o" where
  command _ = "Display abalable commands"

instance Registered "scissors" where
  command _ = "Scissors"

instance Registered "-Dplayer_name" where
  command _ = "Display PlayerName"

instance Registered "-Splayer_name" where
  command _ = "Set PlayerName"

instance Registered "-Denemy_name" where
  command _ = "Display EnemyName"

instance Registered "-Senemy_name" where
  command _ = "Ser EnemyName"

instance Registered "-Dturn" where
  command _ = "Display trun"

instance Registered "-Access" where
  command _ = "Access"



-- deprecated ------------------------------------------------------

--data Command a = forall l. Registered l => Relate a (Proxy l)

{-

lookupRegistered :: Eq c => [Command c] -> c -> Maybe String
lookupRegistered [] _ = Nothing
lookupRegistered ((Relate x p):xs) c = if x == c then Just (command p) else lookupRegistered xs c

lookupFromRegistered :: [Command c] -> String -> Maybe c
lookupFromRegistered [] _ = Nothing
lookupFromRegistered ((Relate x p):xs) s = if command p == s then Just x else lookupFromRegistered xs s

toCommand :: forall c. (Eq c, CommandObj c) => c -> Maybe String
toCommand = lookupRegistered symbols
fromCommand :: forall c. CommandObj c => String -> Maybe c
fromCommand = lookupFromRegistered symbols 

-}

-----------------------------------------------------------------------


data Command' s a = forall l. (KnownSymbol l, Registered l) => Relate (Lens' s a) (Proxy l)

lookupRegisteredSS :: Eq s => [Command' s s] -> s -> Last [(String, String)]
lookupRegisteredSS = lookup_
  where
    lookup_ :: Eq s => [Command' s s] -> s -> Last [(String, String)]
    lookup_ [] _ = Last Nothing
    lookup_ ((Relate l p) : xs) ans = if (ans ^. l) == ans then Last (Just [(symbolVal &&& command) p]) else lookup_ xs ans

lookupFromRegisteredSS :: [Command' s s] -> String -> s -> Last s
lookupFromRegisteredSS = lookup_
  where
    lookup_ :: [Command' s s] -> String -> s -> Last s
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if symbolVal p == s then Last (Just $ ans ^. l) else lookup_ xs s ans

lookupRegisteredSB :: Eq s => [Command' s Bool] -> s -> Last [(String, String)]
lookupRegisteredSB = lookup_ []
  where
    lookup_ :: Eq s => [(String, String)] -> [Command' s Bool] -> s -> Last [(String, String)]
    lookup_ a ((Relate l p) : xs) ans = if ans ^. l 
      then lookup_ ((symbolVal &&& command) p : a) xs ans
      else lookup_ a xs ans
    lookup_ [] [] _ = Last Nothing
    lookup_ a [] _ = Last $ Just a

lookupFromRegisteredSB :: [Command' s Bool] -> String -> s -> Last Bool
lookupFromRegisteredSB = lookup_
  where
    lookup_ :: [Command' s Bool] -> String -> s -> Last Bool
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if symbolVal p == s then Last (Just $ ans ^. l) else lookup_ xs s ans

isRegistered_ :: [Command' s a] -> String -> Bool
isRegistered_ [] _ = False
isRegistered_ ((Relate l p) : xs) s = symbolVal p == s || isRegistered_ xs s


class CommandObj s a | s -> a where
  symbols :: [Command' s a]
  lookupRegistered :: s -> Last [(String, String)]
  lookupFromRegisteredA :: String -> s -> Last a

instance CommandObj Answer Answer where
  symbols = 
    [
      Relate yes_ (Proxy :: Proxy "y"), 
      Relate no_ (Proxy :: Proxy "n")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols


instance CommandObj RPS RPS where
  symbols = 
    [
      Relate rock_ (Proxy :: Proxy "rock"), 
      Relate paper_ (Proxy :: Proxy "paper"),
      Relate scissors_ (Proxy :: Proxy "scissors")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols


instance CommandObj Direction Direction where
  symbols = 
    [
      Relate left_ (Proxy :: Proxy "l"),
      Relate right_ (Proxy :: Proxy "r"),
      Relate down_ (Proxy :: Proxy "d"),
      Relate up_ (Proxy :: Proxy "u")
    ]

  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols

instance CommandObj Ex Ex where
  symbols = 
    [
      Relate moveTwo_ (Proxy :: Proxy "w"),
      Relate trans_ (Proxy :: Proxy "t"),
      Relate flash_ (Proxy :: Proxy "f")
    ]
  
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegisteredA = lookupFromRegisteredSS symbols


instance CommandObj Extra Bool where
  symbols = 
    [
      Relate moveTwo (Proxy :: Proxy "w"),
      Relate trans (Proxy :: Proxy "t"),
      Relate flash (Proxy :: Proxy "f")
    ]
  
  lookupRegistered = lookupRegisteredSB symbols
  lookupFromRegisteredA = lookupFromRegisteredSB symbols

data Descriptor s = 
  Descriptor 
  {
    _tag :: String,
    _elem :: s 
  } deriving Eq

tag :: Lens' (Descriptor s) String
tag = lens (\(Descriptor t _) -> t) (\d t -> d {_tag=t})

element :: Lens' (Descriptor [s]) [s]
element = lens (\(Descriptor _ e) -> e) (\d e -> d {_elem=e})

commandPrint :: String -> String -> String
commandPrint a b = a <> ": " <> b

instance (Show s, CommandObj s a) => Show (Descriptor [s]) where -- fmap ~ xs is bad implemention
  show (Descriptor t xs) = t <> enum xs  
    where
      enum xs = intercalate " or " (uncurry commandPrint  . (maybe ("", "") head . getLast . lookupRegistered) <$> xs) 

instance Show (Descriptor Extra) where
  show (Descriptor t ex) = t <> enum ex
    where 
      enum ex = intercalate " or " $ uncurry commandPrint <$> (fromMaybe [] . getLast . lookupRegistered) ex

class Description d where
  descript :: d -> Descriptor d 

instance Description [Answer] where
  descript = Descriptor "Answer - " 

instance Description [Direction] where
  descript = Descriptor "Direction - "

instance Description [RPS] where
  descript = Descriptor "Rock Scissors Paper GO!!! - "

instance Description [Ex] where
  descript = Descriptor "Extra - "

instance Description Extra where
  descript = Descriptor "Extra - "

printBoard :: Point -> Int -> IO () 
printBoard p d = undefined

size :: Size
size = 5

enemyHP :: Int
enemyHP = 3

data Config = Config 
  {
    _boardSize :: Int,
    _maxEnemyHp :: Int 
  } deriving (Show, Eq)

boardSize :: Lens' Config Int
boardSize = lens (\(Config b _) -> b) (\c b -> c {_boardSize=b})

maxEnemyHp :: Lens' Config Int
maxEnemyHp = lens (\(Config _ h) -> h) (\c h -> c {_maxEnemyHp=h})

config = Config size enemyHP

initPlayerPos :: Point
initPlayerPos = Point 0 0

initEnemyPos :: Point
initEnemyPos = Point size size

initPlayerExtra :: Extra
initPlayerExtra = Extra True False True

initPlayer :: Player
initPlayer = Player "" initPlayerPos True initPlayerExtra

dictionaryPath :: String
dictionaryPath = "AAAAAAAA.txt" -- set dictionary file path (latter ...)

randomEnemyName :: Dictionary
randomEnemyName = Dictionary "" "" -- I want to extract from above path , but sorry latter ...

initEnemy :: Enemy
initEnemy = Enemy randomEnemyName initEnemyPos (size*2) False enemyHP

randomPoint :: Point -- I want to implement warp point, but sorry latter ...
randomPoint = Point 0 0

randomIOPoint :: IO Point
randomIOPoint = Point <$> randomRIO (0, size) <*> randomRIO (0, size)

randomIODir :: IO Direction
randomIODir = 
  let range = (fromEnum (minBound :: Direction), fromEnum (maxBound :: Direction)) :: (Int, Int) 
  in toEnum <$> randomRIO range

randomIORPS :: IO RPS
randomIORPS = 
  let range = (fromEnum (minBound :: RPS), fromEnum (maxBound :: RPS)) :: (Int, Int) 
  in toEnum <$> randomRIO range

initWorld :: World
initWorld = World 0 randomPoint initPlayer initEnemy []

tell' :: String -> Game () -- log on Game monad
tell' s = logs %= (s:)

gameInit :: Game ()
gameInit = do
  liftIO $ putStrLn "Hello, please tell me your name"
  name_ <- liftIO getLine
  (player . name) .= name_
  tell' $ "Hello, " <> name_

gameStart :: Game ()
gameStart = do
  name_ <- use (player . name)
  tell' $ "Log of " <> name_
  mapM_ (liftIO . putStrLn)  
    [
      "hello " <> name_ <> "!!", 
      "Ok. Start Game",
      "This game is on " <> show size <> "*" <> show size <> " board.",
      "Player (you) is (0,0), Enemy is " <> show (size, size) <> " now.",
      "In this situation, you should not meet Enemy on board, if then Bang!!",
      "",
      "",
      "Good luck!",
      "",
      ""
    ]

playerAction :: Game ()
playerAction = do
  flag <- use (player . extraFlag)
  playerPos <- use (player . pos)
  ex_ <- use (player . extra)
  c <- ask 
  let directions = executable  (c ^. boardSize) playerPos
      exs = extraToEx ex_
      description = do
        mapM_ (liftIO . putStrLn) 
          (
            [
              "",
              "Now, you are " <> show (playerPos ^. x, playerPos ^. y) <> " .",
              "Your commnad is ",
              show $ descript directions
            ]
            ++
            ["Or \n" ++ show (descript exs) | flag]
          ) :: Game ()
      
      action = do
        input <- liftIO getLine
        let isMoveCommand = lookupFromRegisteredA input (def :: Direction)
            isExCommand = lookupFromRegisteredA input (def :: Ex)
        case getLast isMoveCommand of
          (Just d) -> if d `elem` directions
            then doMove d 
            else (liftIO .putStrLn) "This command is not executable" >> description >> action
          Nothing -> if flag 
            then
              case getLast isExCommand of
                (Just e) -> if e `elem` exs
                  then doEx e 
                  else (liftIO .putStrLn) "This command is not executable" >> description >> action
                Nothing -> (liftIO . putStrLn) "I don't know what you say" >> action
            else (liftIO . putStrLn) "Where you move ?" >> description >> action 

        return () :: Game ()
        where
          doMove :: Direction -> Game ()
          doMove d = do
            tell' $ "Player : select " <> show d
            (player . pos) %= dirToFunctional d
            (playerPos, enemyPos) <- (,) <$> use (player . pos) <*> use (enemy . pos)
            if playerPos == enemyPos
              then (liftIO . putStrLn) "Here is where enemy is!" >> tell' "Player : killed" >> gameEnd
              else (liftIO . putStrLn) "Succes! Enemy is not here"
          doEx :: Ex -> Game ()
          doEx MoveTwo = do
            tell' $ "Player : select" <> show MoveTwo
            (liftIO . putStrLn) "This turn, you can move Two Steps."
            (player . extraFlag) .= False
            (player . extra . moveTwo) .= False
            playerAction >> (liftIO . putStrLn) "You can move one more" >> playerAction

          doEx Trans = do
            tell' $ "Player : select" <> show Trans
            (liftIO . putStrLn) "This turn, you can move where you want"
            
            -- latter ..

            (player . extraFlag) .= False
            (player . extra . trans) .= False
            playerAction

          doEx Flash = do
            tell' $ "Player : select" <> show Flash
            enemyPos <- use (enemy . pos)
            mapM_ (liftIO . putStrLn) 
              [
                "Flash !",
                "Then world is bright...",
                "",
                "Enemy is at ",
                show (enemyPos ^. x, enemyPos ^. y) <> " you see"
              ]
            (player . extraFlag) .= False
            (player . extra . flash) .= False
            (liftIO . putStrLn) "Move !" >> playerAction

      in description >> action


enemyAction :: Game ()
enemyAction = do
  (playerPos, enemyPos) <- (,) <$> use (player . pos) <*> use (enemy . pos)
  (enemy . dis) .= dist playerPos enemyPos
  (liftIO . putStrLn) "Enemy moves .."
  (enemy . pos) %= (dirToFunctional . vectorToDir) (playerPos - enemyPos)
  judge 
    where
      judge :: Game ()
      judge = do
        (playerPos, enemyPos) <- (,) <$> use (player . pos) <*> use (enemy . pos)
        let dis_ = dist playerPos enemyPos
        if dist playerPos enemyPos == 0  
          then (liftIO . putStrLn) "Player is caught!"  >> tell' "Player : status Have got" >> doRPS
          else (enemy . dis) .= dis_ >> (liftIO . putStrLn) "You are still alive .." 
    
      doRPS :: Game ()
      doRPS = do
        mapM_(liftIO . putStrLn) 
          [
            "Rock Scissors Paper Stage!",
            "If you win then you can escape !",
            "",
            "Please input",
            show $ descript [Rock, Paper, Scissors]  
          ]
        tell' "--Rock Paper Scissors Stage--"
        input <- liftIO getLine
        let isRPS = getLast $ lookupFromRegisteredA input (def :: RPS)
        case isRPS of
          Just rps -> do
            tell' $ "Player : select " <> show rps 
            randomRps <- liftIO randomIORPS
            tell' $ "Enemy : select " <> show randomRps
            mapM_ (liftIO . putStrLn) 
             [
               "You are .. " <> show rps,
               "..",
               "Enemy is .. " <> show randomRps
             ]
            case rpsGo rps randomRps of
              Win -> (liftIO . putStrLn) "You Win !" >> tell' "Player : Win" >> winnerAction
              Draw -> (liftIO . putStrLn) "Draw .. Again !" >> tell' "Player : Draw" >> doRPS
              Defeat -> (liftIO . putStrLn) "You Defeat !" >> tell' "Player : Defeat" >> gameEnd
          _ -> (liftIO . putStrLn) "Invalid command" >> doRPS
        where
          winnerAction :: Game ()
          winnerAction = do
            (enemy . hp) %= minusOne
            (liftIO . putStrLn) "Enemy is damaged .."
            h <- use (enemy . hp)
            when (h == 0) (tell' "Enemy : status Dead .." >> grandEnd)
            (liftIO . putStrLn) "Enemty is still alive .."

updateWorld :: Game ()
updateWorld = do
  isFinish <- use (enemy . goal)
  if isFinish 
    then gameEnd
    else do
      ex_ <- use (player .extra)
      let exs = extraToEx ex_
          hasEx = not $ null exs
      when hasEx ((player . extraFlag) .= True)
      t <- use turn
      liftIO $ putStrLn $ show t <> " trun is over. Next,"
      turn %= (+1)
      warpPoint .= randomPoint

gameLoop :: Game ()
gameLoop = do
  loop
  where
    loop :: Game ()
    loop = do
      t <- use turn
      tell' $ "turn is " <> show t
      distance <- use (enemy . dis)
      playerPos <- use (player . pos)
      mapM_ (liftIO . putStrLn) 
        [
          "  __ Turn is " <> show t <> " __  ",
          "",
          "",
          "Enemy is " <> show distance <> " seperated from " <> "(" <> show (playerPos ^. x) <> "," <> show (playerPos ^. y) <> ")"
              
        ]
      playerAction
      enemyAction
      updateWorld
      loop


qContinue :: Game ()
qContinue = do
  s <- liftIO getLine
  let isYes = getLast (lookupFromRegisteredA s Yes) :: Maybe Answer
      isNo =  getLast (lookupFromRegisteredA s No) :: Maybe Answer
  case isYes of
    Just Yes -> continueYes
    _ -> case isNo of
      Just No -> end
      _ -> (liftIO . putStrLn) "I don't no what you say." >> qContinue
  where
    continueYes :: Game ()
    continueYes = do
      name_ <- use (player . name)
      mapM_ (liftIO . putStrLn)
        [
          "Ok, you change player name " <> "(" <> name_ <> ")" <> " ?",
          show $ descript [Yes, No]  
        ]
      continueQRename
      where
        continueQRename :: Game ()
        continueQRename = do
          s <- liftIO getLine
          let isYes = getLast (lookupFromRegisteredA s Yes) :: Maybe Answer
              isNo =  getLast (lookupFromRegisteredA s No) :: Maybe Answer
          case isYes of
            Just Yes -> continueRename
            _ -> case isNo of
              Just No -> continueSameName
              _ -> (liftIO . putStrLn) "I don't what you say" >> continueQRename
          where
            continueRename :: Game ()
            continueRename = put initWorld >> game
            continueSameName :: Game ()
            continueSameName = do
              name_ <- use (player . name)
              put initWorld
              (player . name) .= name_
              gameStart >> gameLoop

    end :: Game ()
    end = do
      name_ <- use (player . name)
      mapM_ (liftIO . putStrLn) 
        [
          "",
          "Ok, Goodbye " <> name_ <> ".",
          "I sleep .."
        ]
      liftIO exitSuccess 

gameEnd :: Game ()
gameEnd = do
  t <- use turn
  mapM_ (liftIO . putStrLn)
    [
      "Finish! You killed ..", 
      "",
      "",
      "Your point is " <> show t <> " !!!",
      "Congratulations !!!",
      "",
      "",
      "Play continue .. ?",
      show $ descript [Yes, No]
    ]
  qContinue

grandEnd :: Game ()
grandEnd = do
  mapM_ (liftIO . putStrLn) 
    [
      "Enemy is defeat ..",
      "..",
      "World is brigter ..",
      "..",
      "You understand where you stand for the first time ..",
      "..",
      "....",
      "This board is the partion of World Wide",
      "..",
      "...",
      "......",
      "Prepare for next adventure .....",
      "......",
      "............"
    ]
  
  tell' "       __fin__"
  ls <- use logs
  mapM_ (liftIO . putStrLn) (reverse ls)
  liftIO exitSuccess 


game :: Game ()
game = gameInit >> gameStart >> gameLoop


run :: Game () -> IO ()
run game = void $ runStateT (runReaderT game config) initWorld


main :: IO ()
main = run game




