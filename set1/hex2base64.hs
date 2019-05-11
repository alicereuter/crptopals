
num2bin :: Int -> [Int]
num2bin x = if x == 0 then [] else  ( num2bin  (div x 2)) ++ [ mod x  2]

pad :: [Int] -> [Int]
pad str = if (length str) < 8 then pad (0:str) else str


chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

convert :: [Int] -> String
convert (a:b:c:d:e:f:xs) = (chars !!( 32*a+16*b+8*c+4*d+2*e+1*f)): convert xs
convert _ = []

test = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"



hex '0' = [0,0,0,0]
hex '1' = [0,0,0,1]
hex '2' = [0,0,1,0]
hex '3' = [0,0,1,1]
hex '4' = [0,1,0,0]
hex '5' = [0,1,0,1]
hex '6' = [0,1,1,0]
hex '7' = [0,1,1,1]
hex '8' = [1,0,0,0]
hex '9' = [1,0,0,1]
hex 'a' = [1,0,1,0]
hex 'b' = [1,0,1,1]
hex 'c' = [1,1,0,0]
hex 'd' = [1,1,0,1]
hex 'e' = [1,1,1,0]
hex 'f' = [1,1,1,1]


decode x = concat $ map hex x

main = do
  let base64 = convert $ decode test
  putStrLn base64
