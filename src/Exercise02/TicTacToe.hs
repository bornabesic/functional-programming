module Exercise02.TicTacToe where

data Player = X | O | Empty
              deriving (Show, Eq)

data GameState p = Won p | InProgress | Invalid
                   deriving (Show, Eq)

data Grid = Grid {
                topLeft :: Player, topMiddle :: Player, topRight :: Player,
                middleLeft :: Player, middleMiddle :: Player, middleRight :: Player,
                bottomLeft :: Player, bottomMiddle :: Player, bottomRight :: Player
            }
            deriving (Show, Eq)

initGrid = Grid Empty Empty Empty
                Empty Empty Empty
                Empty Empty Empty

gridWonByX = Grid X O     Empty
                  O X     O
                  O Empty X

gameState :: Grid -> (GameState Player)
gameState grid | tl == ml && ml == bl = whoWon tl -- left column
               | tm == mm  && mm == bm = whoWon tm -- middle column
               | tr == mr && mr == br = whoWon tr -- right column
               | tl == tm && tm == tr = whoWon tl -- top row
               | ml == mm && mm == mr = whoWon ml -- middle row
               | bl == bm && bm == br = whoWon bl -- bottom row
               | tl == mm && mm == br = whoWon tl -- left-right diagonal
               | tr == mm && mm == bl = whoWon tr -- right-left diagonal
               | otherwise = InProgress
               where tl = topLeft grid
                     ml = middleLeft grid
                     bl = bottomLeft grid
                     tm = topMiddle grid
                     mm = middleMiddle grid
                     bm = bottomMiddle grid
                     tr = topRight grid
                     mr = middleRight grid
                     br = bottomRight grid

                     whoWon :: Player -> (GameState Player)
                     whoWon X = Won X
                     whoWon O = Won O
                     whoWon _ = InProgress


-- TODO profit margin ?
