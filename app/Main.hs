{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lens -- import from src/Lens.hs

import Control.Monad.State

import Data.List
import Data.Monoid
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
  }

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

right :: Lens' Direction Direction 
right = lens id (const (const RIGHT))

left :: Lens' Direction Direction 
left = lens id (const (const LEFT))

down :: Lens' Direction Direction 
down = lens id (const (const DOWN))

up :: Lens' Direction Direction 
up = lens id (const (const UP))

executable :: Point -> [Direction]
executable p = 
  let xs = []
      addLeft = if p ^. x > 0 then LEFT : xs else xs
      addRight = if p ^. x < size then RIGHT : addLeft else addLeft
      addDown = if p ^. y > 0 then DOWN : addRight else addRight
      addUp = if p ^. y < size then UP : addDown else addDown
      in addUp

data Answer = Yes | No deriving (Eq, Show)

yes :: Lens' Answer Answer 
yes = lens id (const (const Yes))

no :: Lens' Answer Answer 
no = lens id (const (const No))

class Registered l where
  command :: Proxy l -> String

instance Registered "y" where
  command _ = "y"

instance Registered "n" where
  command _ = "n"

instance Registered "l" where
  command _ = "l"

instance Registered "r" where
  command _ = "r"

instance Registered "d" where
  command _ = "d"

instance Registered "u" where
  command _ = "u"

instance Registered "w" where
  command _ = "w"

instance Registered "t" where
  command _ = "t"

instance Registered "f" where
  command _ = "f"

-- deprecated ------------------------------------------------------

--data Command a = forall l. Registered l => Relate a (Proxy l)

{-

lookupRegistered :: Eq c => [Command c] -> c -> Maybe String
lookupRegistered [] _ = Nothing
lookupRegistered ((Relate x p):xs) c = if x == c then Just (command p) else lookupRegistered xs c

lookupFromRegistered :: [Command c] -> String -> Maybe c
lookupFromRegistered [] _ = Nothing
lookupFromRegistered ((Relate x p):xs) s = if command p == s then Just x else lookupFromRegistered xs s -}

{-toCommand :: forall c. (Eq c, CommandObj c) => c -> Maybe String
toCommand = lookupRegistered symbols
fromCommand :: forall c. CommandObj c => String -> Maybe c
fromCommand = lookupFromRegistered symbols 

-}

-----------------------------------------------------------------------


data Command' s a = forall l. Registered l => Relate (Lens' s a) (Proxy l)

lookupRegisteredSS :: Eq s => [Command' s s] -> s -> Last String
lookupRegisteredSS = lookup_
  where
    lookup_ :: Eq s => [Command' s s] -> s -> Last String
    lookup_ [] _ = Last Nothing
    lookup_ ((Relate l p) : xs) ans = if ans ^. l == ans then Last (Just $ command p) else lookup_ xs ans

lookupFromRegisteredSS :: [Command' s s] -> String -> s -> Last s
lookupFromRegisteredSS = lookup_
  where
    lookup_ :: [Command' s s] -> String -> s -> Last s
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if command p == s then Last (Just $ ans ^. l) else lookup_ xs s ans

lookupRegisteredSB :: Eq s => [Command' s Bool] -> s -> Last String
lookupRegisteredSB = lookup_
  where
    lookup_ :: Eq s => [Command' s Bool] -> s -> Last String
    lookup_ [] _ = Last Nothing
    lookup_ ((Relate l p) : xs) ans = if ans ^. l then Last (Just $ command p) else lookup_ xs ans

lookupFromRegisteredSB :: [Command' s Bool] -> String -> s -> Last Bool
lookupFromRegisteredSB = lookup_
  where
    lookup_ :: [Command' s Bool] -> String -> s -> Last Bool
    lookup_ [] _ _ = Last Nothing
    lookup_ ((Relate l p) : xs) s ans = if command p == s then Last (Just $ ans ^. l) else lookup_ xs s ans


class CommandObj s a | s -> a where
  symbols :: [Command' s a]
  lookupRegistered :: s -> Last String
  lookupFromRegistered :: String -> s -> Last a


instance CommandObj Answer Answer where
  symbols = 
    [
      Relate yes (Proxy :: Proxy "y"), 
      Relate no (Proxy :: Proxy "n")
    ]
    
  lookupRegistered = lookupRegisteredSS symbols
  lookupFromRegistered = lookupFromRegisteredSS symbols


instance CommandObj Direction Direction where
  symbols = 
    [
      Relate left (Proxy :: Proxy "l"),
      Relate right (Proxy :: Proxy "r"),
      Relate down (Proxy :: Proxy "d"),
      Relate up (Proxy :: Proxy "u")
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


class Description d where
  descript :: d -> String



{-
class Description d where
  descript :: d -> String

instance Description Extra where
  descript ex = 
    let xs = [(ex ^. moveTwo, "Move Two Step! (w)"), (ex ^. trans, "Transporte! (t)"), (ex ^. flash, "Flash! (f)")]
        in "Extra (command) : " <> intercalate " or " (show . snd <$> filter fst xs)

instance Description [Direction] where
  descript d = "Move (command) : " <> intercalate " or " (fmap show d)
-}

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
            descript directions,
            "Or,", 
            descript ex
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




