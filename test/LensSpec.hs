module LensSpec where

import Test.Hspec
import Lens

import Control.Monad.State

main :: IO ()
main = hspec spec



data Point = Point {_x :: Int, _y :: Int} deriving (Show, Eq)

x :: Lens' Point Int
x = lens (\(Point x _) -> x) (\point x -> point{_x=x})

y :: Lens' Point Int
y = lens (\(Point _ y) -> y) (\point y -> point{_y=y})

instance Num Point where
  (Point px py) + (Point qx qy) = Point (px+qx) (py+qy)
  (Point px py) * (Point qx qy) = Point (px*qx) (py*qy)
  abs (Point px py) = Point (abs px) (abs py)
  signum (Point px py) = Point (signum px) (signum py)
  fromInteger i = Point (fromInteger i) (fromInteger i)
  negate (Point px py) = Point (negate px) (negate py)

dist :: Point -> Point -> Int
dist p q = let r = abs (p - q) in r ^. x + r ^. y




data Foo = Foo {_name :: String, _point :: Point} deriving (Show, Eq)

name :: Lens' Foo String
name = lens (\(Foo name _) -> name) (\foo n -> foo{_name=n})

point :: Lens' Foo Point
point = lens (\(Foo _ point) -> point) (\foo p -> foo{_point=p})


p :: Point
p = Point 1 2

foo :: Foo
foo = Foo "ball" p 

initPoint :: State Foo ()
initPoint = point .= Point 0 0 

getPointX :: State Foo Int
getPointX = use (point . x)

moveOneToY :: State Foo ()
moveOneToY = (point . y) %= (+1)

data Dir = LEFT | RIGHT deriving (Show, Eq)
left_ :: Lens' Dir Dir
left_ = lens (const LEFT) (\dir s -> s)

right_ :: Lens' Dir Dir
right_ = lens id (\dir s -> s)

spec :: Spec
spec = do
  describe "test Lens" $ do
      it "test to" $ do
        view (to show) True `shouldBe` "True"
      
      it "test make Lens1" $ do
        view x p `shouldBe` 1

      it "test make Lens2" $ do
        view point foo `shouldBe` p

      it "test make Lens3" $ do
        foo ^. (point . y) `shouldBe` 2

      it "test make Lens4" $ do
        (name .~ "rect") foo `shouldBe` foo{_name="rect"}

      it "test lens on context of state 1" $ do
        runState initPoint  foo `shouldBe` ((), foo {_point = Point 0 0})

      it "test lens on context of state 2" $ do
        evalState getPointX  foo `shouldBe` 1

      it "test lens on context of state 3" $ do
        runState moveOneToY  foo `shouldBe` ((), foo {_point = Point 1 3})

      it "test geter Light is light" $ do
        RIGHT ^. left_ `shouldBe` LEFT

      it "test (0, 0) - (5, 5) == - (5, 5)" $ do
        Point 0 0 - Point 5 5 `shouldBe` - Point 5 5

      it "test dist from (0,0) to (5, 5) == 10" $ do
        dist (Point 0 0) (Point 5 5) `shouldBe` 10