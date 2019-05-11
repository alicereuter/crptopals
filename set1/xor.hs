
xor 1 1 = 0
xor 1 0 = 1
xor 0 1 = 1
xor 0 0 = 0


chars = "0123456789abcdef"


hex2int y = length $ takeWhile (\x -> x /= y) chars

pad :: [Int] -> [Int]
pad str = if (length str) < 4 then pad (0:str) else str

num2bin :: Int -> [Int]
num2bin x = if x == 0 then [] else  ( num2bin  (div x 2)) ++ [ mod x  2]


n2b = pad . num2bin . hex2int

app :: [Int] -> [Int] -> [Int]
app (x:xs) (y:ys) = xor x y : app xs ys
app _ _ = []

str1 = "1c0111001f010100061a024b53535009181c"
str2 = "686974207468652062756c6c277320657965"

hex2bin x = concat $ map n2b x

bin2hex (a:b:c:d:xs) = chars !! (a*8+b*4+c*2+d*1) : bin2hex xs
bin2hex [] = []


main = do
  let z = app (hex2bin str1) (hex2bin str2)
  print $  bin2hex z

