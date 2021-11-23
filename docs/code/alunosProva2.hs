import Data.Char (toLower)
import Data.List (sort)

ePangrama :: [Char] -> Bool
ePangrama s = verificaPangrama s ['a'..'z'] ['A'..'Z']

verificaPangrama l [] [] = True
verificaPangrama l x y = if elem (head x) l == True || elem (head y) l then verificaPangrama l (tail x) (tail y) else False

--- >>> ePangrama ['a'..'z']
-- True

-- >>> iterate (^2) 3
-- ProgressCancelledException

--- >>> count 1 [1,2,3]
