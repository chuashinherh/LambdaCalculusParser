{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LambdaParser where

import Parser
import Data.Lambda
import Data.Builder
import Data.Char

-- You can add more imports if you need them

-- Remember that you can (and should) define your own functions, types, and
-- parser combinators. Each of the implementations for the functions below
-- should be fairly short and concise.


{-|
    Part 1
-}

-- | Exercise 1

-- | Parses a string representing a lambda calculus expression in long form
--
-- >>> parse longLambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse longLambdaP "(λx.(λy.xy(xx)))"
-- Result >< \xy.xy(xx)
--
-- >>> parse longLambdaP "(λx(λy.x))"
-- UnexpectedChar '('
-- test :: Parser Lambda
-- test = do
--     is '('
--     is 'λ'
--     x <- var
--     is '.'
--     x2 <- var
--     is ')'
--     pure (build $ lam x (term x2))

-- <expr> ::= <var>
-- 		   | <var> <expr>
-- 		   | "\\" <expr> "." <expr>
--         | "(" <expr> ")"
--         | "(" <expr> ")" <expr>
-- <var> ::= [a-z]

-- <expr> ::= <atom> <expr> | <atom>
-- <atom> ::= "λ" <expr> "." <expr> | <var> | "(" <expr> ")"
-- <var> ::= [a-z]

expr :: Parser Builder
expr = chainl1 atomLong (pure ap)

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op = p >>= rest
    where rest a = (do
                      f <- op
                      b <- p
                      rest (f a b)
                   ) ||| pure a

atomLong :: Parser Builder
atomLong = lamLong ||| term1 ||| paren

var :: Parser Char
var = satisfy isAlpha ||| is '_'

lamLong :: Parser Builder
lamLong = do is 'λ'
             x <- var
             is '.'
             lam x <$> expr

term1 :: Parser Builder
term1 = term <$> var

paren :: Parser Builder
paren = do is '('
           e <- expr
           is ')'
           pure e

longLambdaP :: Parser Lambda
longLambdaP = build <$> expr

-- | Parses a string representing a lambda calculus expression in short form
--
-- >>> parse shortLambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse shortLambdaP "λxy.xy(xx)"
-- Result >< \xy.xy(xx)
--
-- >>> parse shortLambdaP "λx.x(λy.yy)"
-- Result >< \x.x\y.yy
--
-- >>> parse shortLambdaP "(λx.x)(λy.yy)"
-- Result >< (\x.x)\y.yy
--
-- >>> parse shortLambdaP "λxyz"
-- UnexpectedEof

-- <exprStart> ::= <atomStart> <exprStart> | <atomStart>
-- <expr2> ::= <atomShortLam> <expr2> | <atomShortLam>
-- <expr3> ::= <atomShortTerm> <expr3> | <atomShortTerm>
-- <atomStart> ::= "λ" <lam2> | <paren2>
-- <atomShortLam> ::= <lam2> | <paren2> | <dot>
-- <atomShortTerm> ::= "λ" <lam2> | <term1> | <paren2>
-- <lam2> ::= <var> <expr2>
-- <dot> ::= "." <expr3>
-- <paren2> ::= "(" <expr3> ")"
-- <term1> ::= <var>
-- <var> ::= [a-z] | [A-Z]

exprStart :: Parser Builder
exprStart = chainl1 atomStart (pure ap)

expr2 :: Parser Builder
expr2 = chainl1 atomShortLam (pure ap)

expr3 :: Parser Builder
expr3 = chainl1 atomShortTerm (pure ap)

atomStart :: Parser Builder
atomStart = (is 'λ' >> lam2) ||| paren2

atomShortLam :: Parser Builder
atomShortLam = lam2 ||| paren2 ||| dot

atomShortTerm :: Parser Builder
atomShortTerm = (is 'λ' >> lam2) ||| term1 ||| paren2

lam2 :: Parser Builder
lam2 = do x <- var
          lam x <$> expr2

dot :: Parser Builder
dot = do is '.'
         expr3

paren2 :: Parser Builder
paren2 = do is '('
            e <- expr3
            is ')'
            pure e

shortLambdaP :: Parser Lambda
shortLambdaP = build <$> exprStart


-- | Parses a string representing a lambda calculus expression in short or long form
-- >>> parse lambdaP "λx.xx"
-- Result >< \x.xx
--
-- >>> parse lambdaP "(λx.xx)"
-- Result >< \x.xx
--
-- >>> parse lambdaP "λx..x"
-- UnexpectedChar '.'
--

lambdaP :: Parser Lambda
lambdaP = shortLambdaP

{-|
    Part 2
-}

-- | Exercise 1

-- IMPORTANT: The church encoding for boolean constructs can be found here -> https://tgdwyer.github.io/lambdacalculus/#church-encodings

-- | Parse a logical expression and returns in lambda calculus
-- >>> lamToBool <$> parse logicP "True and False"
-- Result >< Just False
--
-- >>> lamToBool <$> parse logicP "True and False or not False and True"
-- Result >< Just True
--
-- >>> lamToBool <$> parse logicP "not not not False"
-- Result >< Just True
--
-- >>> parse logicP "True and False"
-- Result >< (\xy.(\btf.btf)xy\_f.f)(\t_.t)\_f.f
--
-- >>> parse logicP "not False"
-- Result >< (\x.(\btf.btf)x(\_f.f)\t_.t)\_f.f
-- >>> lamToBool <$> parse logicP "if True and not False then True or True else False"
-- Result >< Just True

spaces1 :: Parser String
spaces1 = list1 space

-- chainr1 function referenced from https://stackoverflow.com/questions/40648175/parse-expression-right-to-left
chainr1 :: Parser a -> Parser (a->a->a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = ((\f y -> f x y) <$> op <*> (p >>= rest))
             ||| pure x

literal :: String -> Parser String
literal l = do spaces
               string l
               pure l

literalOp :: String -> Parser String
literalOp l = do spaces1
                 string l
                 spaces1
                 pure l

exprLogic :: Parser Builder
exprLogic = chainl1 exprOr (pure ap)

exprOr :: Parser Builder
exprOr = chainl1 exprAnd or2

exprAnd :: Parser Builder
exprAnd = chainl1 exprNot and2

exprNot :: Parser Builder
exprNot = chainr1 atomTest (pure ap)

atomTest :: Parser Builder
atomTest = true ||| false ||| not1
           ||| do spaces
                  is '('
                  x <- exprLogic
                  is ')'
                  pure x
           ||| do spaces
                  a <- if1
                  b <- exprLogic
                  literalOp "then"
                  c <- exprLogic
                  literalOp "else"
                  d <- exprLogic
                  pure $ a `ap` b `ap` c `ap` d

true :: Parser Builder
true = do literal "True"
          pure $ boolToLam True

false :: Parser Builder
false = do literal "False"
           pure $ boolToLam False

if1 :: Parser Builder
if1 = do literal "if"
         pure if2

if2 :: Builder
if2 = lam 'b' (lam 't' (lam 'f' (term 'b' `ap` term 't' `ap` term 'f')))

and2 :: Parser(Builder -> Builder -> Builder)
and2 = do literalOp "and"
          pure and3

and3 :: Builder -> Builder -> Builder
and3 a b = lam 'x' (lam 'y' (if2 `ap` term 'x' `ap` term 'y' `ap` boolToLam False)) `ap` a `ap` b

or2 :: Parser (Builder -> Builder -> Builder)
or2 = do literalOp "or"
         pure or3

or3 :: Builder -> Builder -> Builder
or3 a b = lam 'x' (lam 'y' (if2 `ap` term 'x' `ap` boolToLam True `ap` term 'y')) `ap` a `ap` b

not1 :: Parser Builder
not1 = do literal "not"
          spaces1
          pure not2

not2 :: Builder
not2 = lam 'x' (if2 `ap` term 'x' `ap` boolToLam False `ap` boolToLam True)

logicP :: Parser Lambda
logicP = build <$> exprLogic

-- | Exercise 2

-- | The church encoding for arithmetic operations are given below (with x and y being church numerals)

-- | x + y = add = λxy.y succ x
-- | x - y = minus = λxy.y pred x
-- | x * y = multiply = λxyf.x(yf)
-- | x ** y = exp = λxy.yx

-- | The helper functions you'll need are:
-- | succ = λnfx.f(nfx)
-- | pred = λnfx.n(λgh.h(gf))(λu.x)(λu.u)
-- | Note since we haven't encoded negative numbers pred 0 == 0, and m - n (where n > m) = 0

-- | Parse simple arithmetic expressions involving + - and natural numbers into lambda calculus
-- >>> lamToInt <$> parse basicArithmeticP "5 + 4"
-- Result >< Just 9
--
-- >>> lamToInt <$> parse basicArithmeticP "5 + 9 - 3 + 2"
-- Result >< Just 13

exprAddMinus :: Parser Builder
exprAddMinus = chainl1 atomBasicArith (add1 ||| minus1)

atomBasicArith :: Parser Builder
atomBasicArith = naturalNo

naturalNo :: Parser Builder
naturalNo = do spaces
               x <- munch1 isDigit
               let y = read x
               pure $ intToLam y

succ1 :: Builder
succ1 = lam 'n' $ lam 'f' $ lam 'x' (term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

pred1 :: Builder
pred1 = lam 'n' $ lam 'f' $ lam 'x' (term 'n' `ap` lam 'g' (lam 'h' (term 'h' `ap` (term 'g' `ap` term 'f'))) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u'))

operator :: Char -> Parser Char
operator c = do spaces
                is c
                pure c

add1 :: Parser (Builder -> Builder -> Builder)
add1 = do operator '+'
          pure add2

add2 :: Builder -> Builder -> Builder
add2 a b = lam 'x' (lam 'y' (term 'y' `ap` succ1 `ap` a)) `ap` a `ap` b

minus1 :: Parser (Builder -> Builder -> Builder)
minus1 = do operator '-'
            pure minus2

minus2 :: Builder -> Builder -> Builder
minus2 a b = lam 'x' (lam 'y' (term 'y' `ap` pred1 `ap` a)) `ap` a `ap` b

basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> exprAddMinus

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

exprArith :: Parser Builder
exprArith = chainl1 exprMult (add1 ||| minus1)

exprMult :: Parser Builder
exprMult = chainl1 exprExp mult1

exprExp :: Parser Builder
exprExp = chainl1 atomArith exp1

atomArith :: Parser Builder
atomArith = naturalNo
            ||| do spaces
                   is '('
                   x <- exprArith
                   is ')'
                   pure x
            ||| do spaces
                   a <- if1
                   b <- exprComp
                   literalOp "then"
                   c <- exprComp
                   literalOp "else"
                   d <- exprComp
                   pure $ a `ap` b `ap` c `ap` d

mult1 :: Parser (Builder -> Builder -> Builder)
mult1 = do operator '*'
           pure mult3

mult2 :: Builder
mult2 = lam 'x' (lam 'y' (lam 'f' (term 'x' `ap` (term 'y' `ap` term 'f'))))

mult3 :: Builder -> Builder -> Builder
mult3 a b = mult2 `ap` a `ap` b

exp1 :: Parser (Builder -> Builder -> Builder)
exp1 = do literal "**"
          pure exp2

exp2 :: Builder -> Builder -> Builder
exp2 a b = lam 'x' (lam 'y' (term 'y' `ap` term 'x')) `ap` a `ap` b

arithmeticP :: Parser Lambda
arithmeticP = build <$> exprArith

-- | Exercise 3

-- | The church encoding for comparison operations are given below (with x and y being church numerals)

-- | x <= y = LEQ = λmn.isZero (minus m n)
-- | x == y = EQ = λmn.and (LEQ m n) (LEQ n m)

-- | The helper function you'll need is:
-- | isZero = λn.n(λx.False)True

-- >>> lamToBool <$> parse complexCalcP "9 - 2 <= 3 + 6"
-- Result >< Just True
--
-- >>> lamToBool <$> parse complexCalcP "15 - 2 * 2 != 2**3 + 3 or 5 * 3 + 1 < 9"
-- Result >< Just False

exprComp :: Parser Builder
exprComp = chainl1 exprComp2 and2

exprComp2 :: Parser Builder
exprComp2 = chainl1 exprComp3 or2

exprComp3 :: Parser Builder
exprComp3 = chainl1 (exprLogic ||| atomComp) (test ||| equalBool)
            ||| chainl1 (exprArith ||| atomComp) (test ||| equal1)

test :: Parser (Builder -> Builder -> Builder)
test = lessOrEqual1 ||| greaterOrEqual1 ||| notEqual1 ||| greater1 ||| less1

atomComp :: Parser Builder
atomComp = do spaces
              is '('
              x <- exprComp
              is ')'
              pure x
           ||| do spaces
                  a <- if1
                  b <- exprComp
                  literalOp "then"
                  c <- exprComp
                  literalOp "else"
                  d <- exprComp
                  pure $ a `ap` b `ap` c `ap` d

isZero :: Builder
isZero = lam 'n' (term 'n' `ap` lam 'x' (boolToLam False) `ap` boolToLam True)

lessOrEqual1 :: Parser (Builder -> Builder -> Builder)
lessOrEqual1 = do literal "<="
                  pure lessOrEqual2

lessOrEqual2 :: Builder -> Builder -> Builder
lessOrEqual2 m n = lam 'm' (lam 'n' (isZero `ap` minus2 m n)) `ap` m `ap` n

equal1 :: Parser (Builder -> Builder -> Builder)
equal1 = do literal "=="
            pure equal2

equal2 :: Builder -> Builder -> Builder
equal2 m n = lam 'm' (lam 'n' (and3 (lessOrEqual2 m n) (lessOrEqual2 n m))) `ap` m `ap` n

equalBool :: Parser (Builder -> Builder -> Builder)
equalBool = do literal "=="
               pure xnor

-- XNOR church encoding modified from XOR church encoding. 
-- Referenced from https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
xnor :: Builder -> Builder -> Builder
xnor m n = not2 `ap` (lam 'm' (lam 'n' (if2 `ap` term 'm' `ap` (not2 `ap` term 'n') `ap` term 'n')) `ap` m `ap` n)

notEqual1 :: Parser (Builder -> Builder -> Builder)
notEqual1 = do literal "!="
               pure notEqual2

notEqual2 :: Builder -> Builder -> Builder
notEqual2 m n = not2 `ap` equal2 m n

greater1 :: Parser (Builder -> Builder -> Builder)
greater1 = do literal ">"
              pure greater2

greater2 :: Builder -> Builder -> Builder
greater2 m n = not2 `ap` lessOrEqual2 m n

less1 :: Parser (Builder -> Builder -> Builder)
less1 = do literal "<"
           pure less2

less2 :: Builder -> Builder -> Builder
less2 m n = greater2 n m

greaterOrEqual1 :: Parser (Builder -> Builder -> Builder)
greaterOrEqual1 = do literal ">="
                     pure greaterOrEqual2

greaterOrEqual2 :: Builder -> Builder -> Builder
greaterOrEqual2 m n = not2 `ap` less2 m n

complexCalcP :: Parser Lambda
complexCalcP = build <$> exprComp


{-|
    Part 3
-}

-- | Exercise 1

-- | The church encoding for list constructs are given below
-- | [] = null = λcn.n
-- | isNull = λl.l(λht.False) True
-- | cons = λhtcn.ch(tcn)
-- | head = λl.l(λht.h) False
-- | tail = λlcn.l(λhtg.gh(tc))(λt.n)(λht.t)
--
-- >>> parse listP "[]"
-- Result >< \cn.n
--
-- >>> parse listP "[True]"
-- Result >< (\htcn.ch(tcn))(\xy.x)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof
null1 :: Builder
null1 = lam 'c' (lam 'n' (term 'n'))

isNull1 :: Builder
isNull1 = lam 'l' (term 'l' `ap` lam 'h' (lam 't' (boolToLam False)) `ap` boolToLam True)

cons1 :: Builder
cons1 = lam 'h' (lam 't' (lam 'c' (lam 'n' (term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n')))))

head1 :: Builder
head1 = lam 'l' (term 'l' `ap` lam 'h' (lam 't' (term 'h')) `ap` boolToLam False)

tail1 :: Builder
tail1 = lam 'l' (lam 'c' (lam 'n' (term 'l' `ap` lam 'h' (lam 't' (lam 'g' (term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')))) `ap` lam 't' (term 'n') `ap` lam 'h' (lam 't' (term 't')))))

exprList :: Parser Builder
exprList = chainr1 atomList (pure ap)

atomList :: Parser Builder
atomList = (spaces >> is '[' >> is ']' >> pure null1)
           ||| do spaces
                  is '['
                  x <- atomList
                  y <- exprList
                  pure $ cons1 `ap` x `ap` y
           ||| openB ||| closeB ||| exprComp ||| exprLogic ||| exprArith

openB :: Parser Builder
openB = do spaces
           is ','
           spaces
           x <- atomList
           y <- exprList
           pure $ cons1 `ap` x `ap` y

closeB :: Parser Builder
closeB = do spaces
            is ']'
            pure null1

listP :: Parser Lambda
listP = build <$> exprList

-- >>> lamToBool <$> parse listOpP "head [True, False, True, False, False]"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "head rest [True, False, True, False, False]"
-- Result >< Just False
--
-- >>> lamToBool <$> parse listOpP "isNull []"
-- Result >< Just True
--
-- >>> lamToBool <$> parse listOpP "isNull [1, 2, 3]"
-- Result >< Just False
exprListOp :: Parser Builder
exprListOp = chainr1 atomListOp (pure ap)

atomListOp :: Parser Builder
atomListOp = exprList ||| tail2 ||| head2 ||| isNull2 ||| cons2

head2 :: Parser Builder
head2 = do literal "head"
           pure head1

tail2 :: Parser Builder
tail2 = do literal "rest"
           pure tail1

isNull2 :: Parser Builder
isNull2 = do literal "isNull"
             pure isNull1

cons2 :: Parser Builder
cons2 = do literal "cons"
           pure cons1

listOpP :: Parser Lambda
listOpP = build <$> exprListOp


-- | Exercise 2

-- | Implement your function(s) of choice below!

-- Factorial Function
-- Church encoding of factorial adapted from https://groups.seas.harvard.edu/courses/cs152/2016sp/lectures/lec08-encodings.pdf
chainl2 :: Parser a -> Parser (a->a) -> Parser a
chainl2 p op = p >>= rest
    where rest a = (do
                      f <- op
                      rest (f a)
                   ) ||| pure a

exprFact :: Parser Lambda
exprFact = build <$> chainl2 naturalNo fact1

zero :: Builder
zero = lam 'f' (lam 'x' (term 'x'))

one :: Builder
one = lam 'f' (lam 'x' (term 'f' `ap` term 'x'))

fact1 :: Parser (Builder -> Builder)
fact1 = do operator '!'
           pure fact2

fact2 :: Builder -> Builder
fact2 n = fact' `ap` fact' `ap` n

fact' :: Builder
fact' = lam 'f' (lam 'n' (if2 `ap` (isZero `ap` term 'n') `ap` one `ap` (mult2 `ap` term 'n' `ap` (term 'f' `ap` term 'f' `ap` (pred1 `ap` term 'n')))))


-- Fibonacci Sequence Function
-- Church encoding of Fibonacci sequence adapted from https://www.cs.cmu.edu/~venkatg/teaching/15252-sp20/notes/lambda-calculus-slides.pdf
fibNaturalNo :: Parser Builder
fibNaturalNo = do spaces
                  x <- munch1 isDigit
                  let y = read x
                  pure $ minus2 (intToLam y) one

fib' :: Builder
fib' = lam 'f' (lam 'n' (if2 `ap` (isZero `ap` term 'n') `ap` zero `ap` (if2 `ap` (isZero `ap` (pred1 `ap` term 'n')) `ap` one `ap` add2 (term 'f' `ap` term 'f' `ap` (pred1 `ap` term 'n')) (term 'f' `ap` term 'f' `ap` (pred1 `ap` (pred1 `ap` term 'n'))))))

fib1 :: Parser Builder
fib1 = do literal "fib"
          pure fib2

fib2 :: Builder
fib2 = fib' `ap` fib'

exprFib :: Parser Lambda
exprFib = build <$> chainl1 (fibNaturalNo ||| fib1) (pure ap)


-- Negative Numbers
-- Church encoding of negative numbers adapted from https://en.wikipedia.org/wiki/Church_encoding#Predicates
pair :: Builder
pair = lam 'x' (lam 'y' (lam 'z' (term 'z' `ap` term 'x' `ap` term 'y')))

first :: Builder
first = lam 'p' (term 'p' `ap` lam 'x' (lam 'y' (term 'x')))

second :: Builder
second = lam 'p' (term 'p' `ap` lam 'x' (lam 'y' (term 'y')))

convert :: Builder
convert = lam 'x' (pair `ap` term 'x' `ap` zero)

negativeNo :: Parser Builder
negativeNo = do operator '-'
                x <- munch1 isDigit
                let y = read x
                pure $ intToLam y

neg :: Builder
neg = lam 'x' (pair `ap` (second `ap` term 'x') `ap` (first `ap` term 'x'))

exprNeg :: Parser Lambda
exprNeg = do x <- negativeNo
             pure $ build $ neg `ap` (convert `ap` x)
