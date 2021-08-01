import Test.QuickCheck

celsius2fahrenheit c = c * 9 / 5 + 32 

fahrenheit2celsius f = (f - 32) * 5/9

prop_C2f2C c = fahrenheit2celsius (celsius2fahrenheit c) == c
