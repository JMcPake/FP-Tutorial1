module Tutorial1 where

import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck

-- Exercise 2:

double :: Int -> Int
double x = x + x

square :: Int -> Int
square x = x ^ 2

-- Exercise 3:

isTriple :: Int -> Int -> Int -> Bool
isTriple a b c = square a + square b == square c


-- Exercise 4:

leg1 :: Int -> Int -> Int
leg1 x y = square x - square y

leg2 :: Int -> Int -> Int
leg2 x y = 2*x*y

hyp :: Int -> Int -> Int
hyp x y = square x + square y

-- Exercise 5:

prop_triple :: Int -> Int -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- Exercise 7:

pic1 :: Picture
pic1 = (knight `beside` invert knight) `above` (invert knight `beside` knight)

pic2 :: Picture
pic2 = (invert knight `beside`  knight) `above` flipV (invert knight `beside` knight)

-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- Exercise 8:

twoAbove :: Picture -> Picture
twoAbove x = invert x `above` x

fourPictures :: Picture -> Picture
fourPictures x = (invert x `beside` x) `above` (invert x `beside` x)

-- Exercise 9:
-- a)

emptyRow :: Picture
emptyRow =  (repeatH 4 (whiteSquare `beside` blackSquare))

-- b)

otherEmptyRow :: Picture
otherEmptyRow =  (repeatH 4 (blackSquare `beside` whiteSquare))

-- c)

middleBoard :: Picture
middleBoard = (repeatV 2 (emptyRow `above` otherEmptyRow))

-- d)

whiteRow :: Picture
whiteRow = (rook `beside` knight `beside` bishop `beside` queen `beside` king `beside` bishop `beside` knight `beside` rook)

blackRow :: Picture
blackRow = (invert (rook `beside` knight `beside` bishop `beside` queen `beside` king `beside` bishop `beside` knight `beside` rook))

-- e)

whitePawn :: Picture
whitePawn = (repeatH 8 pawn)

blackPawn :: Picture
blackPawn = (repeatH 8 (invert pawn))

populatedBoard :: Picture
populatedBoard = (blackRow `over` emptyRow) `above` (blackPawn `over` otherEmptyRow) `above` middleBoard `above` (whitePawn `over` emptyRow) `above` (whiteRow `over` otherEmptyRow)



