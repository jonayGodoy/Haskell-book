module StateExampleInFuntional where

import Control.Monad
import Control.Monad.Trans.State


data TurnstileState = Locked | Unlocked deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
 let (a1, s1) = coin s0
     (a2, s2) = push s1
     (a3, s3) = push s2
     (a4, s4) = coin s3
     (a5, s5) = push s4
 in ([a1, a2, a3, a4, a5], s5)


-- the same with State
-- newtype State s a = State { runState :: s -> (a, s) }


coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

--runState coinS Locked == (Thank,Unlocked)


mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

-- runState mondayS Locked
--([Thank,Open,Tut,Thank,Open],Locked)

mondayS' =
  coinS >>= (\ a1 ->
    pushS >>= (\ a2 ->
      pushS >>= (\ a3 ->
        coinS >>= (\ a4 ->
          pushS >>= (\ a5 ->
            return [a1, a2, a3, a4, a5] )))))

-- runState mondayS' Locked
-- ([Thank,Open,Tut,Thank,Open],Locked)
