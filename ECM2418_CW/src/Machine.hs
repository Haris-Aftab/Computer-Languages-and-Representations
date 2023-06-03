{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Machine
  ( Vname,
    Val,
    State,
    Instr (..),
    Stack,
    Config,
    iexec,
    exec,
  )
where

import Data.Map

--TODO Task 1.1
type Vname = String

--TODO Task 1.2
type Val = Int

--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr
  = LOADI Val
  | LOAD Vname
  | ADD
  | STORE Vname
  | JMP Val
  | JMPLESS Val
  | JMPGE Val
  deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (counter, state, stack) = (counter + 1, state, x : stack)

iexec (LOAD v) (counter, state, stack) = (counter + 1, state, state ! v : stack)

iexec ADD (counter, state, stack) = (counter + 1, state, hd + head tl : tail tl)
  where
    hd = head stack
    tl = tail stack

iexec (STORE v) (counter, state, stack) = (counter + 1, insert v (head stack) state, tail stack)

iexec (JMP i) (counter, state, stack) = (counter + i + 1, state, stack)

iexec (JMPLESS i) (counter, state, stack)
  | head (tail stack) < head stack = (counter + i + 1, state, tail(tail stack))
  | otherwise = (counter + 1, state, tail(tail stack))

iexec (JMPGE i) (counter, state, stack)
  | head stack <= head(tail stack) = (counter + i + 1, state, tail(tail stack))
  | otherwise = (counter + 1, state, tail(tail stack))

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (counter, state, stack) = (counter, state, stack)
exec (x:xs) (counter, state, stack) = exec xs (iexec x(counter, state, stack))