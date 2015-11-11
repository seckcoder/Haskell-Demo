module Memoization.EditDistance where


import Debug.Trace

{-
This demo shows how memoization emerges naturally from the lazy
evaluation rule: value in haskell only evaluated as needed and at most once.
-}
demo1 = do
    let v = print "hello"
    v -- this is an action, when the action "executed", it will print hello
    v

demo2 = do
    let v = trace "hello" 3
    putStr "world "
    print v -- "hello" is only printed once. (Also, note that trace is not referential transpart, so the print order is not guaranteed)
    print v
    

fib n =
    case fibs !! 10 of
        v -> do
            print v
            case fibs !! 4 of
                v -> do
                    print v
      where fibs = trace "fibs..." $ 0 : 1 : zipWith (\ a b -> trace "plus" (a+b)) fibs (drop 1 fibs)

main = do
    fib 10