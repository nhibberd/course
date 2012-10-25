import Data.List

fib =
  unfoldr (\(n, o) -> Just (n, (o, n+o))) (0, 1)

fib2 = 0:1:zipWith (+) fib2 (tail fib2)