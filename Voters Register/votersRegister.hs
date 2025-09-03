-- module Main where 

type UserName = String 
type UserId   = String 
type Age      = String
type Address = String 

data Details  =  Details 
 {userName :: UserName
 ,userId   :: UserId
 ,age      :: Age
 ,address  :: Address
 }deriving (Show)
 
 
data Register = Register {details :: [Details] } deriving (Show)
 
instance Semigroup Register where
  (<>) (Register a) (Register b) = Register(a <> b)
  
instance Monoid Register where
  mempty  = Register []
  mappend (Register a) (Register b) = Register(a <> b)

instance Semigroup Details where 
  (<>) (Details x y z w) (Details f g h j) = Details (x <> f) (y <> g) (z <> h) (w <> j)

instance Monoid Details where
  mempty = Details "" "" "" ""
  
addToDetails ::  UserName -> UserId -> Age -> Address -> Details
addToDetails x y z w = Details {userName = x, userId = y, age = z, address = w}
 
addDetailsToRegister :: Details -> Register
addDetailsToRegister x = Register {details = [x]}

addNewDetailsToRegister :: Details -> Register -> Register
addNewDetailsToRegister x (Register xs) = Register (x : xs)
 
main :: IO()
main = do
  let x = "Israel, "
  let y = "12342324, "
  let z = "67, "
  let w = "76 ghtsbvys, "
  
  let details1 = addToDetails x y z w
  let details2 = addToDetails "yu, " "123, " "34, " "jhkjh, "
  let details3 = addToDetails "Victor, " "123, " "56, " "Lagos, "

  let register1 = addDetailsToRegister details1  
  let register2 = addNewDetailsToRegister details2 register1
  let register3 = addNewDetailsToRegister details3 register2
  
  print $ mempty <> register1 <> register2 <> register3
  print $ mappend register1 register2 
  print $ mempty <> details1 <> details2

  
 
 
 
 