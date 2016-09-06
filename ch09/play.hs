strlen :: IO ()
strlen= do 
	putStr "Enter a string: "
	xs <- getLine
	putStrLn $ "The string has "++(show (length xs))++ " characters"
	 
beep :: IO ()
beep =putStr "\BEL"

cls :: IO ()
cls =putStr "\ESC[2J"

-- By convention, the position of each character on the screen is given by a pair (x , y ) of positive integers, with (1, 1) being the top-left corner.
type Pos = (Int,Int)

-- moves the cursor to a given position
-- the cursor is a marker that indicates where the next character displayed will appear
goto :: Pos -> IO ()
goto(x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H") -- 有点厉害.. \ESC[magic]

writeat :: Pos -> String -> IO () 
writeat p xs = do 
	goto p
	putStr xs

seqn :: [IO a]->IO() 
seqn [] = return () 
seqn (a:as) = do 
	a
	seqn as

-- ORZ
-- box :: [String]
-- box = ["+---------------+",
-- 	"|               |", 
-- 	"+---+---+---+---+", 
-- 	"| q | c | d | = |", 
-- 	"+---+---+---+---+", 
-- 	"| 1 | 2 | 3 | + |", 
-- 	"+---+---+---+---+", 
-- 	"| 4 | 5 | 6 | - |", 
-- 	"+---+---+---+---+", 
-- 	"| 7 | 8 | 9 | * |", 
-- 	"+---+---+---+---+", 
-- 	"| 0 | ( | ) | / |", 
-- 	"+---+---+---+---+"]
-- buttons :: [Char ]
-- buttons = standard ++ extra
-- 	where
-- 		standard = "qcd=123+456-789*0()/" 
-- 		extra = "QCD \ESC\BS\DEL\n"
-- showbox :: IO ()
-- showbox = seqn [writeat (1,y) xs | (y,xs) <- zip [1..13] box]
-- display :: String → IO ()
-- display xs = 
-- 	do 
-- 		writeat (3, 2) "            "
-- 		writeat (3,2) (reverse (take 13 (reverse xs)))

-- calc :: String -> IO ()
-- calc xs = do 
-- 	display xs
-- 	c <- getCh
-- 	if elem c buttons then
-- 		process c xs
-- 	else
-- 		do 
-- 			beep
-- 			calc xs























