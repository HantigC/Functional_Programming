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

-- Calculate e^x with error 
aproximateExp x error = sum (takeWhile (>= error) (taylorEx x))


