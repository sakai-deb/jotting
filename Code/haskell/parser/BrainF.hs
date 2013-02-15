import Data.Char (chr, ord)
--
-- Define Brainfuck syntax
--
data Br = Next | Prev | Succ | Pred | Out | In | Loop [Br] deriving (Show, Eq)

--
-- Parser
--
translate :: String -> [Br]
translate = fst . pretranslate
--
-- Translater for translate
--
(+++) :: a -> ([a], b) -> ([a], b)
(+++) s (t, str) = (s:t, str)
pretranslate :: String -> ([Br], String)
pretranslate ('>':str) = Next +++ pretranslate str
pretranslate ('<':str) = Prev +++ pretranslate str
pretranslate ('+':str) = Succ +++ pretranslate str
pretranslate ('-':str) = Pred +++ pretranslate str
pretranslate ('.':str) = Out +++ pretranslate str
pretranslate (',':str) = In +++ pretranslate str
-- Loop
pretranslate ('[':str) = let (bf, xs) = pretranslate str in Loop bf +++ pretranslate xs
pretranslate (']':str) = ([], str)
-- Ignoring invarid chars
pretranslate (_:str) = pretranslate str
pretranslate [] = ([], [])

--
-- State of Brainfuck : consist of array and pos.
--
type State = ([Int], Int, [Int])
-- Execution Brainfuck syntax
exec :: [Br] -> IO([Br], State)
exec bf = do
  (bf, state) <- step bf ([], 0, [])
  -- putStrLn $ show (bf, state)
  exec' bf state

exec' bf state
  | bf == []  = return ([], state)
  | otherwise = do
      (bf', state') <- step bf state
      -- putStrLn $ show (bf', state')
      -- putStrLn $ show state'
      exec' bf' state'

step :: [Br] -> State -> IO ([Br], State)
step (Next:bf) (xs, x, [])            = return (bf, (x:xs, 0, []))
step (Next:bf) (xs, x, s:ys)          = return (bf, (x:xs, s, ys))
step (Prev:bf) state@([], x, ys)      = return (bf, state)
step (Prev:bf) (s:xs, x, ys)          = return (bf, (xs, s, x:ys))
step (Succ:bf) (xs, x, ys)            = return (bf, (xs, x + 1, ys))
step (Pred:bf) (xs, x, ys)            = return (bf, (xs, x - 1, ys))
step (Loop []:bf) state@(xs, 0, ys)   = return (bf, state)
step br@(Loop s:bf) state@(xs, x, ys) = do
  (_, st@(xs', x', ys')) <- (exec' s state)
  if ( x' == 0 )
     then return (bf, st)
     else return (br, st)
step (Out:bf) state@(xs, x, ys)       = do
  putChar (chr x)
  return (bf, state)
step (In:bf) (xs, x, ys)              = do
  s <- getChar
  return (bf, (xs,ord  s, ys))
step [] state                         = return ([], state)

--
-- Interpret & Execution
--
interpreter :: String -> IO()
interpreter str = (exec $ translate str) >> (return ())

main :: IO()
main = do
   contents <- readFile "BrainFuck.txt"
   interpreter contents
