-- Totoian Marius-Horatiu
-- Aproximate the exponential function with a given error using taylor series

-- List with only x
listOnlyx x = x : (listOnlyx x)

-- List with 1, x, x^2, x^3,....
listpower::Float -> [Float]
listpower x = 1.0 : zipWith (*) (listOnlyx x) (listpower x)

-- List of factorials
factorials = 1.0 : zipWith (*) factorials [1..]

-- Taylor series, sum of x^n/(n!)
taylorEx x = zipWith (/) (listpower x) factorials

-- calculate the termen x^n/n! which is < error and return n
getN:: Float -> Float -> Int -> Int -> Float -> Int
getN pow x factorial n error
    | pow / fromIntegral(factorial) < error = n 
    | otherwise  = getN (pow * x) x (factorial * (n + 1)) (n+1) error


-- Calculate e^x with error 
aproximateExp x error = sum (take (getN x x 1 1 error) (taylorEx x))


