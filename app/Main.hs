{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lens -- import from src/Lens.hs

import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Arrow
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

type Name = String

data Player =
  Player 
  {
    _playerName :: Name, 
    _playerPoint :: Point,
    _extra :: Extra
  } deriving (Show, Eq)

playerName :: Lens' Player Name
playerName = lens (\(Player n _ _) -> n) (\obj n -> obj {_playerName=n})

playerPoint :: Lens' Player Point
playerPoint = lens (\(Player _ p _) -> p) (\obj p -> obj {_playerPoint=p})

extra :: Lens' Player Extra
extra = lens (\(Player _ _ e) -> e) (\obj e -> obj {_extra=e})

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

data Direction = RIGHT | LEFT | UP | DOWN deriving (Eq, Show)

right_ :: Lens' Direction Direction 
right_ = lens id (const (const RIGHT))

left_ :: Lens' Direction Direction 
left_ = lens id (const (const LEFT))

down_ :: Lens' Direction Direction 
down_ = lens id (const (const DOWN))

up_ :: Lens' Direction Direction 
up_ = lens id (const (const UP))

executable :: Point -> [Direction]
executable p = 
  let xs = []
      addLeft = if p ^. x > 0 then LEFT : xs else xs
      addRight = if p ^. x < size then RIGHT : addLeft else addLeft
      addDown = if p ^. y > 0 then DOWN : addRight else addRight
      addUp = if p ^. y < size then UP : addDown else addDown
      in addUp

data Answer = Yes | No deriving (Eq, Show)

yes_ :: Lens' Answer Answer 
yes_ = lens id (const (const Yes))

no_ :: Lens' Answer Answer 
no_ = lens id (const (const No))

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
    lookup_ ((Relate l p) : xs) ans = if ans ^. l == ans then Last (Just [(symbolVal &&& command) p]) else lookup_ xs ans

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
    lookup_ [] _ _ = Last Nothing
    lookup_ a [] _ = Last $ Just a
    lookup_ a ((Relate l p) : xs) ans = if ans ^. l 
      then lookup_ ((symbolVal &&& command) p : a) xs ans
      else lookup_ a xs ans

lookupFromRegisteredSB :: [Command' s Bool] -> String -> s -> Last Bool
lookupFromRegisteredSB = lookup_
  where
    lookup_ :: [Command' s Bool] -> String -> s -> Last Bool
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if symbolVal p == s then Last (Just $ ans ^. l) else lookup_ xs s ans


class CommandObj s a | s -> a where
  symbols :: [Command' s a]
  lookupRegistered :: s -> Last [(String, String)]
  lookupFromRegistered :: String -> s -> Last a


instance CommandObj Answer Answer where
  symbols = 
    [
      Relate yes_ (Proxy :: Proxy "y"), 
      Relate no_ (Proxy :: Proxy "n")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegistered = lookupFromRegisteredSS symbols


instance CommandObj Direction Direction where
  symbols = 
    [
      Relate left_ (Proxy :: Proxy "l"),
      Relate right_ (Proxy :: Proxy "r"),
      Relate down_ (Proxy :: Proxy "d"),
      Relate up_ (Proxy :: Proxy "u")
    ]

  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegistered = lookupFromRegisteredSS symbols

instance CommandObj Extra Bool where
  symbols = 
    [
      Relate moveTwo (Proxy :: Proxy "w"),
      Relate trans (Proxy :: Proxy "t"),
      Relate flash (Proxy :: Proxy "f")
    ]
  
  lookupRegistered = lookupRegisteredSB symbols
  lookupFromRegistered = lookupFromRegisteredSB symbols

data Descriptor s = 
  Descriptor 
  {
    _tag :: String,
    _elem :: s 
  } deriving Eq

tag :: Lens' (Descriptor s) String
tag = lens (\(Descriptor t _) -> t) (\d t -> d {_tag=t})

elem :: Lens' (Descriptor [s]) [s]
elem = lens (\(Descriptor _ e) -> e) (\d e -> d {_elem=e})

instance (CommandObj s a) => Show (Descriptor [s]) where
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
initPlayer = Player "" initPlayerPos initPlayerExtra

dictionaryPath :: String
dictionaryPath = "gene-utf8.txt" -- set dictionary file path. 

randomEnemyName :: Dictionary
randomEnemyName = Dictionary "" ""

initEnemy :: Enemy
initEnemy = Enemy randomEnemyName initEnemyPos (size*2) False

randomPoint :: Point
randomPoint = Point 0 0

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
  ex <- use (player . extra)
  let directions = executable playerPos
      
      description = do
        mapM_ (liftIO . putStrLn) 
          [
            "",
            "Now, you are " <> show (playerPos ^. x, playerPos ^. y) <> " .",
            "Your commnad is ",
            show $ descript directions,
            "Or,", 
            show $ descript ex
          ] :: Game ()
      
      action = do
        input <- liftIO getLine
        playerAction input
        return () :: Game ()
          where
            playerAction :: String -> Game ()
            playerAction s = do 
              void $ return ex
      
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
      "y or n"
    ]
  yesOrNo <- liftIO getLine
  loopOrEnd yesOrNo
  where 
    loopOrEnd :: String -> Game ()
    loopOrEnd "n" = do
      name_ <- use (player . name)
      mapM_ (liftIO . putStrLn) 
        [
          "",
          "Ok, Goodbye " <> name_ <> ".",
          "I sleep .."
        ]
    
    loopOrEnd "y" = do
      name_ <- use (player . name)
      mapM_ (liftIO . putStrLn)
        [
          "Ok, you change player name " <> "(" <> name_ <> ")" <> " ?",
          "y or n" 
        ]
      isRename <- liftIO getLine
      continueRename isRename 
      where
        continueRename :: String -> Game ()
        continueRename "n" = do
          name_ <- use (player . name)
          put initWorld
          (player . name) .= name_
          gameStart >> gameLoop
          
        continueRename "y" = do
          game

        continueRename _ = do
          liftIO $ putStrLn "plese y or n, I'm not clever"
          yesOrNo <- liftIO getLine
          continueRename yesOrNo

    loopOrEnd _ = do
      liftIO $ putStrLn "plese y or n, I'm not clever"
      yesOrNo <- liftIO getLine
      loopOrEnd yesOrNo


game :: Game ()
game = gameInit >> gameStart >> gameLoop


run :: Game () -> IO ()
run game = fst <$> runStateT game initWorld


main :: IO ()
main = run game




