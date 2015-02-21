module Main where

--- Akkerman function
akk :: Int -> Int -> Int
akk m n = if m < 0
	    then -1
	    else if n < 0
		   then -1
		   else let
			  go a b = if a == 0
				     then b + 1
				     else if a > 0 && b == 0
					    then let a' = a - 1
						     b' = 1
						 in go a' b'
					  else
					    let a1  = a - 1
						b1' = b - 1
						b1  = go a b1'
					    in go a1 b1
			in go m n 

main = print (akk 3 2)
