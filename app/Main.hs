{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE  AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lens -- import from src/Lens.hs

import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Arrow

import System.Random
import Data.Proxy
import GHC.TypeLits



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
    _goal :: Bool
  } deriving (Show, Eq)

enemyName :: Lens' Enemy Dictionary
enemyName = lens (\(Enemy n _ _ _) -> n) (\e n -> e {_enemyName = n})

enemyPoint :: Lens' Enemy Point
enemyPoint = lens (\(Enemy _ p _ _) -> p) (\e p -> e {_enemyPoint = p})

dis :: Lens' Enemy Int
dis = lens (\(Enemy _ _ r _) -> r) (\e r -> e {_dis = r})

goal :: Lens' Enemy Bool
goal = lens (\(Enemy _ _ _ g) -> g) (\e g -> e {_goal = g})

data World = 
  World 
  {
    _turn :: Int,
    _warpPoint :: Point, 
    _player :: Player, 
    _enemy :: Enemy
  } deriving (Show, Eq)

turn :: Lens' World Int
turn = lens (\(World t _ _ _) -> t) (\w t -> w {_turn=t})

warpPoint :: Lens' World Point
warpPoint = lens (\(World _ p _ _) -> p) (\w p -> w {_warpPoint=p})

player :: Lens' World Player
player = lens (\(World _ _ p _) -> p) (\w p -> w {_player=p})

enemy :: Lens' World Enemy
enemy = lens (\(World _ _ _ e) -> e) (\w e -> w {_enemy=e})

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

type Game = StateT World IO

type Size = Int

data Direction = RIGHT | LEFT | UP | DOWN deriving (Eq, Ord, Enum, Show, Bounded)

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

executable :: Point -> [Direction]
executable p = 
  let xs = []
      addLeft = if p ^. x > 0 then LEFT : xs else xs
      addRight = if p ^. x < size then RIGHT : addLeft else addLeft
      addDown = if p ^. y > 0 then DOWN : addRight else addRight
      addUp = if p ^. y < size then UP : addDown else addDown
      in addUp

data Answer = Yes | No deriving (Eq, Ord, Enum, Show, Bounded)

yes_ :: Lens' Answer Answer 
yes_ = lens (const Yes) (\ans s -> s)

no_ :: Lens' Answer Answer 
no_ = lens (const No) (\ans s -> s)

data RPS = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show, Bounded)

rock_ :: Lens' RPS RPS 
rock_ = lens (const Rock) (\rps s -> s)

paper_ :: Lens' RPS RPS 
paper_ = lens (const Paper) (\rps s -> s)

scissors_ :: Lens' RPS RPS 
scissors_ = lens (const Scissors) (\rps s -> s)

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

instance (Show s, CommandObj s a) => Show (Descriptor [s]) where -- fmap ~ xs is bad implemention
  show (Descriptor t xs) = t <> enum xs  
    where
      enum xs = intercalate " or " (uncurry (<>) . (maybe ("", "") head . getLast . lookupRegistered) <$> xs) 

instance Show (Descriptor Extra) where
  show (Descriptor t ex) = t <> enum ex
    where 
      enum ex = intercalate " or " $ uncurry (<>) <$> (fromMaybe [] . getLast . lookupRegistered) ex

class Description d where
  descript :: d -> Descriptor d 

instance Description [Answer] where
  descript = Descriptor "Answer : " 

instance Description [Direction] where
  descript = Descriptor "Direction : "

instance Description [Ex] where
  descript = Descriptor "Extra : "

instance Description Extra where
  descript = Descriptor "Extra : "

printBoard :: Point -> Int -> IO () 
printBoard p d = undefined

size :: Size
size = 5

initPlayerPos :: Point
initPlayerPos = Point 0 0

initEnemyPos :: Point
initEnemyPos = Point size size

initPlayerExtra :: Extra
initPlayerExtra = Extra True True True

initPlayer :: Player
initPlayer = Player "" initPlayerPos True initPlayerExtra

dictionaryPath :: String
dictionaryPath = "gene-utf8.txt" -- set dictionary file path. 

randomEnemyName :: Dictionary
randomEnemyName = Dictionary "" ""

initEnemy :: Enemy
initEnemy = Enemy randomEnemyName initEnemyPos (size*2) False

randomPoint :: Point
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
initWorld = World 0 randomPoint initPlayer initEnemy

gameInit :: Game ()
gameInit = do
  liftIO $ putStrLn "Hello, please tell me your name"
  name_ <- liftIO getLine
  (player . name) .= name_

gameStart :: Game ()
gameStart = do
  name_ <- use (player . name)
  mapM_ (liftIO . putStrLn)  
    [
      "hello " <> name_ <> "!!", 
      "Ok. Start Game",
      "This game is on " <> show size <> "*" <> show size <> " board.",
      "Player (you) is (0, 0), Enemy is " <> show (size, size) <> " now.",
      "In this situation, you should not meet Enemy on board, if then Bang!!",
      "",
      "",
      "Good luck!",
      "",
      ""
    ]

playerAction :: Game ()
playerAction = do
  playerPos <- use (player . pos)
  ex_ <- use (player . extra)
  let directions = executable playerPos
      exs = extraToEx ex_
      description = do
        mapM_ (liftIO . putStrLn) 
          [
            "",
            "Now, you are " <> show (playerPos ^. x, playerPos ^. y) <> " .",
            "Your commnad is ",
            show $ descript directions,
            "Or,", 
            show $ descript exs
          ] :: Game ()
      
      action = do
        flag <- use (player . extraFlag)
        input <- liftIO getLine
        let isMoveCommand = lookupFromRegisteredA input LEFT
            isExCommand = lookupFromRegisteredA input MoveTwo
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
            (player . pos) %= dirToFunctional d
          doEx :: Ex -> Game ()
          doEx e = do
            return ()

      in description >> action


enemyAction :: Game ()
enemyAction = do
  return ()

updateWorld :: Game ()
updateWorld = do
  isFinish <- use (enemy . goal)
  if isFinish 
    then gameEnd
    else do
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
            continueRename = game
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



game :: Game ()
game = gameInit >> gameStart >> gameLoop


run :: Game () -> IO ()
run game = fst <$> runStateT game initWorld


main :: IO ()
main = run game




