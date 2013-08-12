--module Cgrun034 where
-- !! fromRational woes

import Data.Ratio -- 1.3

{-
main = do
  putStrLn (show (tinyFloat == t_f))
  where
    t_f :: Float
    t_f = fromRationalX (toRational tinyFloat)
-}

main = putStrLn (show f)

f = tinyFloat == t_f
  where
    t_f :: Float
    t_f = fromRationalX (toRational tinyFloat)

fromRationalX :: (RealFloat a) => Rational -> a
fromRationalX r =
	let
	    h = ceiling (huge `asTypeOf` x)
	    b = toInteger (floatRadix x)
	    x = fromRat 0 r
	    fromRat e0 r' =
		let d = denominator r'
		    n = numerator r'
	        in  if d > h then
		       let e = integerLogBase b (d `div` h) + 1
		       in  fromRat (e0-e) (n % (d `div` (b^e)))
		    else if abs n > h then
		       let e = integerLogBase b (abs n `div` h) + 1
		       in  fromRat (e0+e) ((n `div` (b^e)) % d)
		    else
		       scaleFloat e0 (rationalToRealFloat {-fromRational-} r')
	in  x

-- Compute the discrete log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
        0
     else
	-- Try squaring the base first to cut down the number of divisions.
        let l = 2 * integerLogBase (b*b) i
	    doDiv :: Integer -> Int -> Int
	    doDiv i l = if i < b then l else doDiv (i `div` b) (l+1)
	in  doDiv (i `div` (b^l)) l


------------

-- Compute smallest and largest floating point values.
tiny :: (RealFloat a) => a
tiny =
	let (l, _) = floatRange x
	    x = encodeFloat 1 (l-1)
	in  x

huge :: (RealFloat a) => a
huge =
	let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in  x

tinyDouble = tiny :: Double
tinyFloat  = tiny :: Float
hugeDouble = huge :: Double
hugeFloat  = huge :: Float

rationalToRealFloat :: (RealFloat a) => Rational -> a

rationalToRealFloat x	=  x'
	where x'    = f e

--		If the exponent of the nearest floating-point number to x
--		is e, then the significand is the integer nearest xb^(-e),
--		where b is the floating-point radix.  We start with a good
--		guess for e, and if it is correct, the exponent of the
--		floating-point number we construct will again be e.  If
--		not, one more iteration is needed.

	      f e   = if e' == e then y else f e'
		      where y	   = encodeFloat (round (x * (1%b)^^e)) e
			    (_,e') = decodeFloat y
	      b	    = floatRadix x'

--		We obtain a trial exponent by doing a floating-point
--		division of x's numerator by its denominator.  The
--		result of this division may not itself be the ultimate
--		result, because of an accumulation of three rounding
--		errors.

	      (s,e) = decodeFloat (fromInteger (numerator x) `asTypeOf` x'
					/ fromInteger (denominator x))

