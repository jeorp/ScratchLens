module Main where

import Lens -- import from src/Lens.hs

import Control.Monad.State


-- example program 

data Point = Point {_x :: Int, _y :: Int}

x :: Lens' Point Int
x = lens (\(Point x _) -> x) (\p x -> p {_x=x})

y :: Lens' Point Int
y = lens (\(Point _ y) -> y) (\p y -> p {_y=y})

data Extra = Extra {_moveTwo :: Bool, _trans :: Bool, _flash :: Bool}

moveTwo :: Lens' Extra Bool
moveTwo = lens (\(Extra b _ _) -> b) (\e b -> e {_moveTwo=b})

trans :: Lens' Extra Bool
trans = lens (\(Extra _ b _) -> b) (\e b -> e {_trans=b})

flash :: Lens' Extra Bool
flash = lens (\(Extra _ _ b) -> b) (\e b -> e {_trans=b})

type Name = String

data Player =
  Player 
  {
    _playerName :: Name, 
    _playerPoint :: Point,
    _extra :: Extra
  }

playerName :: Lens' Player Name
playerName = lens (\(Player n _ _) -> n) (\obj n -> obj {_playerName=n})

playerPoint :: Lens' Player Point
playerPoint = lens (\(Player _ p _) -> p) (\obj p -> obj {_playerPoint=p})

extra :: Lens' Player Extra
extra = lens (\(Player _ _ e) -> e) (\obj e -> obj {_extra=e})

data Enemy = 
  Enemy 
  {
    _enemyName :: Name, 
    _enemyPoint :: Point, 
    _remain :: Int, _goal :: Bool
  }

enemyName :: Lens' Enemy String
enemyName = lens (\(Enemy n _ _ _) -> n) (\e n -> e {_enemyName = n})

enemyPoint :: Lens' Enemy Point
enemyPoint = lens (\(Enemy _ p _ _) -> p) (\e p -> e {_enemyPoint = p})

remain :: Lens' Enemy Int
remain = lens (\(Enemy _ _ r _) -> r) (\e r -> e {_remain = r})

goal :: Lens' Enemy Bool
goal = lens (\(Enemy _ _ _ g) -> g) (\e g -> e {_goal = g})

data World = World {_turn :: Int, _player :: Player, _enemy :: Enemy}

turn :: Lens' World Int
turn = lens (\(World t _ _) -> t) (\w t -> w {_turn=t})

player :: Lens' World Player
player = lens (\(World _ p _) -> p) (\w p -> w {_player=p})

enemy :: Lens' World Enemy
enemy = lens (\(World _ _ e) -> e) (\w e -> w {_enemy=e})

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
  name = enemyName

type Game = StateT World IO

type Size = Int

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

initEnemy :: Enemy
initEnemy = Enemy "" initEnemyPos (size*2) False

initWorld :: World
initWorld = World 0 initPlayer initEnemy

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

gameLoop :: Game ()
gameLoop = do
  loop
  where
    loop :: Game ()
    loop = do
      isFinish <- use (enemy . goal)
      if isFinish 
          then gameEnd 
          else do
            t <- use turn
            liftIO $ putStrLn $ "  __ Turn is " <> show t <> " __  "

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
          (player . playerName) .= name_
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




