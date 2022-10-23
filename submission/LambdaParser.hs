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

-- Parser that creates a Builder expression from the lambda expression.
exprLong :: Parser Builder
exprLong = chainl1 atomLong (pure ap)

-- chainl1 function obtained from FIT2102 Course Notes.
-- Handle repeated chains of operators of unknown length that are left associative.
chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op = p >>= rest
    where rest a = (do
                      f <- op
                      b <- p
                      rest (f a b)
                   ) ||| pure a

-- Parser that combines various parsers together to process the characters in the lambda expression.
atomLong :: Parser Builder
atomLong = lamLong ||| term1 ||| parenLong

-- Parser that parses variables in the lambda expression.
var :: Parser Char
var = satisfy isAlpha ||| is '_'

-- Parser for the parameters of the lambda expression.
lamLong :: Parser Builder
lamLong = do is 'λ'
             x <- var
             is '.'
             lam x <$> exprLong

-- Parser for the function body of the lambda expression.
term1 :: Parser Builder
term1 = term <$> var

-- Parser to parse lambda expressions with parentheses.
parenLong :: Parser Builder
parenLong = do is '('
               e <- exprLong
               is ')'
               pure e

-- Parser that parses a long lambda expression.
longLambdaP :: Parser Lambda
longLambdaP = build <$> exprLong

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

-- Parser that creates a Builder expression for the start of the lambda expression.
-- This is to make sure that the lambda expression starts with a 'λ' character.
exprStart :: Parser Builder
exprStart = chainl1 atomStart (pure ap)

-- Parser that creates a Builder expression for the parameters from the lambda expression.
exprShortLam :: Parser Builder
exprShortLam = chainl1 atomShortLam (pure ap)

-- Parser that creates a Builder expression from the function body of the lambda expression.
exprShortTerm :: Parser Builder
exprShortTerm = chainl1 atomShortTerm (pure ap)

-- Parser that combines various parsers together to process the characters 
-- at the start of the lambda expression.
atomStart :: Parser Builder
atomStart = (is 'λ' >> lamShort) ||| parenShort

-- Parser that combines various parsers together to process the characters of the parameters 
-- of the lambda expression.
atomShortLam :: Parser Builder
atomShortLam = lamShort ||| parenShort ||| dot

-- Parser that combines various parsers together to process the characters in the 
-- function body of the lambda expression.
atomShortTerm :: Parser Builder
atomShortTerm = (is 'λ' >> lamShort) ||| term1 ||| parenShort

-- Parser for the parameters of the lambda expression.
lamShort :: Parser Builder
lamShort = do x <- var
              lam x <$> exprShortLam

-- Parser that parses a dot character.
dot :: Parser Builder
dot = do is '.'
         exprShortTerm

-- Parser to parse lambda expressions with parentheses.
parenShort :: Parser Builder
parenShort = do is '('
                e <- exprShortTerm
                is ')'
                pure e

-- Parser that parses a short lambda expression.
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

-- Parser that parses a lambda expression.
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

-- Parser that parses one or more space characters.
spaces1 :: Parser String
spaces1 = list1 space

-- chainr1 function referenced from https://stackoverflow.com/questions/40648175/parse-expression-right-to-left
-- Handle repeated chains of operators of unknown length that are right associative.
chainr1 :: Parser a -> Parser (a->a->a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = ((\f y -> f x y) <$> op <*> (p >>= rest))
             ||| pure x

-- Parser that parses a literal such as "True", "False", etc.
literal :: String -> Parser String
literal l = do spaces
               string l
               pure l

-- Parser that parses an operator that requires a space before and after it 
-- such as "and", "or", etc.
literalOp :: String -> Parser String
literalOp l = do spaces1
                 string l
                 spaces1
                 pure l

-- Parser that creates a Builder expression from the logical expression.
exprLogic :: Parser Builder
exprLogic = chainl1 exprOr (pure ap)

-- Parser that creates a Builder expression for the sections in the logical expression 
-- that contain "or".
exprOr :: Parser Builder
exprOr = chainl1 exprAnd orParser

-- Parser that creates a Builder expression for the sections in the logical expression 
-- that contain "and".
exprAnd :: Parser Builder
exprAnd = chainl1 exprNot andParser

-- Parser that creates a Builder expression for the sections in the logical expression 
-- that contain "not".
-- Using chainr1 here instead of chainl1 is because "not" is right associative.
exprNot :: Parser Builder
exprNot = chainr1 atomLogic (pure ap)

-- Parser that combines various parsers together to process the characters in the logical expression.
atomLogic :: Parser Builder
atomLogic = true ||| false ||| notParser ||| parenLogic
           ||| do spaces
                  a <- ifParser
                  b <- exprLogic
                  literalOp "then"
                  c <- exprLogic
                  literalOp "else"
                  d <- exprLogic
                  pure $ a `ap` b `ap` c `ap` d

-- Parser to parse logical expressions with parentheses.
parenLogic :: Parser Builder
parenLogic = do spaces
                is '('
                x <- exprLogic
                is ')'
                pure x

-- Parser to parse the literal "True".
true :: Parser Builder
true = do literal "True"
          (pure . boolToLam) True

-- Parser to parse the literal "False".
false :: Parser Builder
false = do literal "False"
           pure $ boolToLam False

-- Parser to parse the operator "if".
ifParser :: Parser Builder
ifParser = do literal "if"
              pure ifBuilder

-- Creates a Builder expression for the operator "if".
ifBuilder :: Builder
ifBuilder = lam 'b' (lam 't' (lam 'f' (term 'b' `ap` term 't' `ap` term 'f')))

-- Parser to parse the operator "and".
andParser :: Parser (Builder -> Builder -> Builder)
andParser = do literalOp "and"
               pure andBuilder

-- Creates a Builder expression for the operator "and".
andBuilder :: Builder -> Builder -> Builder
andBuilder = ap . ap (lam 'x' (lam 'y'
    (ifBuilder `ap` term 'x' `ap` term 'y' `ap` boolToLam False)))

-- Parser to parse the operator "or".
orParser :: Parser (Builder -> Builder -> Builder)
orParser = do literalOp "or"
              pure orBuilder

-- Creates a Builder expression for the operator "or".
orBuilder :: Builder -> Builder -> Builder
orBuilder = ap . ap (lam 'x' (lam 'y'
    (ifBuilder `ap` term 'x' `ap` boolToLam True `ap` term 'y')))

-- Parser to parse the operator "not".
notParser :: Parser Builder
notParser = do literal "not"
               spaces1
               pure notBuilder

-- Creates a Builder expression for the operator "not".
notBuilder :: Builder
notBuilder = lam 'x' (ifBuilder `ap` term 'x' `ap` boolToLam False `ap` boolToLam True)

-- Parser that parses a logical expression.
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

-- Parser that creates a Builder expression from the basic arithmetic expression.
exprBasicArith :: Parser Builder
exprBasicArith = chainl1 atomBasicArith (addParser ||| minusParser)

-- Parser that processes the characters in the basic arithmetic expression.
atomBasicArith :: Parser Builder
atomBasicArith = naturalNo

-- Creates a Builder expression of a natural number.
naturalNo :: Parser Builder
naturalNo = do spaces
               x <- munch1 isDigit
               let y = read x
               pure $ intToLam y

-- Creates a Builder expression for the successor of a natural number.
succ1 :: Builder
succ1 = lam 'n' $ lam 'f' $ lam 'x' (term 'f' `ap` (term 'n' `ap` term 'f' `ap` term 'x'))

-- Creates a Builder expression for the predecessor of a natural number.
pred1 :: Builder
pred1 = lam 'n' $ lam 'f' $ lam 'x' (term 'n' `ap` lam 'g' (lam 'h'
    (term 'h' `ap` (term 'g' `ap` term 'f'))) `ap` lam 'u' (term 'x') `ap` lam 'u' (term 'u'))

-- Parser that parses an operator such as "+", "-", etc.
operator :: Char -> Parser Char
operator c = do spaces
                is c
                pure c

-- Parser to parse the operator "+".
addParser :: Parser (Builder -> Builder -> Builder)
addParser = do operator '+'
               pure addBuilder

-- Creates a Builder expression for the operator "+".
-- addBuilder :: Builder -> Builder -> Builder
-- addBuilder a b = lam 'x' (lam 'y' (term 'y' `ap` succ1 `ap` a)) `ap` a `ap` b

addBuilder :: Builder -> Builder -> Builder
addBuilder = ap . ap (lam 'x' (lam 'y' (term 'y' `ap` succ1) `ap` term 'x'))

-- Parser to parse the operator "-".
minusParser :: Parser (Builder -> Builder -> Builder)
minusParser = do operator '-'
                 pure minusBuilder

-- Creates a Builder expression for the operator "-".
minusBuilder :: Builder -> Builder -> Builder
minusBuilder = ap . ap (lam 'x' (lam 'y' (term 'y' `ap` pred1 `ap` term 'x')))

-- Parser that parses a basic arithmetic expression.
basicArithmeticP :: Parser Lambda
basicArithmeticP = build <$> exprBasicArith

-- | Parse arithmetic expressions involving + - * ** () and natural numbers into lambda calculus
-- >>> lamToInt <$> parse arithmeticP "5 + 9 * 3 - 2**3"
-- Result >< Just 24
--
-- >>> lamToInt <$> parse arithmeticP "100 - 4 * 2**(4-1)"
-- Result >< Just 68

-- Parser that creates a Builder expression for the sections in the arithmetic expression 
-- that contain "+" or "-".
exprArith :: Parser Builder
exprArith = chainl1 exprMult (addParser ||| minusParser)

-- Parser that creates a Builder expression for the sections in the arithmetic expression 
-- that contain "*".
exprMult :: Parser Builder
exprMult = chainl1 exprExp multParser

-- Parser that creates a Builder expression for the sections in the arithmetic expression 
-- that contain "**".
exprExp :: Parser Builder
exprExp = chainl1 atomArith expParser

-- Parser that combines various parsers together to process the characters in the arithmetic expression.
atomArith :: Parser Builder
atomArith = naturalNo ||| parenArith
            ||| do spaces
                   a <- ifParser
                   b <- exprComp
                   literalOp "then"
                   c <- exprComp
                   literalOp "else"
                   d <- exprComp
                   pure $ a `ap` b `ap` c `ap` d

-- Parser to parse arithmetic expressions with parentheses.
parenArith :: Parser Builder
parenArith = do spaces
                is '('
                x <- exprArith
                is ')'
                pure x

-- Parser to parse the operator "*".
multParser :: Parser (Builder -> Builder -> Builder)
multParser = do operator '*'
                pure multBuilder2

multBuilder :: Builder
multBuilder = lam 'x' (lam 'y' (lam 'f' (term 'x' `ap` (term 'y' `ap` term 'f'))))

-- Creates a Builder expression for the operator "*".
multBuilder2 :: Builder -> Builder -> Builder
multBuilder2 = ap . ap multBuilder

-- Parser to parse the operator "**".
expParser :: Parser (Builder -> Builder -> Builder)
expParser = do literal "**"
               pure expBuilder

-- Creates a Builder expression for the operator "**".
expBuilder :: Builder -> Builder -> Builder
expBuilder = ap . ap (lam 'x' (lam 'y' (term 'y' `ap` term 'x')))

-- Parser that parses an arithmetic expression.
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

-- Parser that creates a Builder expression for the sections in the complex calculation expression 
-- that contain "and".
exprCompAnd :: Parser Builder
exprCompAnd = chainl1 exprCompOr andParser

-- Parser that creates a Builder expression for the sections in the complex calculation expression 
-- that contain "or".
exprCompOr :: Parser Builder
exprCompOr = chainl1 exprComp orParser

-- Parser that creates a Builder expression for the sections in the complex calculation expression 
-- that contain comparison operators.
exprComp :: Parser Builder
exprComp = chainl1 (exprLogic ||| atomComp) (compOp ||| equalBoolParser)
            ||| chainl1 (exprArith ||| atomComp) (compOp ||| equalIntParser)

-- Parser that parses a comparison operator such as "==", "<=", etc.
compOp :: Parser (Builder -> Builder -> Builder)
compOp = lessOrEqualParser ||| greaterOrEqualParser
         ||| notEqualParser ||| greaterParser ||| lessParser

-- Parser that combines various parsers together to process the characters in the complex calculation expression.
atomComp :: Parser Builder
atomComp = parenComp
           ||| do spaces
                  a <- ifParser
                  b <- exprComp
                  literalOp "then"
                  c <- exprComp
                  literalOp "else"
                  d <- exprComp
                  pure $ a `ap` b `ap` c `ap` d

-- Parser to parse complex calculation expressions with parentheses.
parenComp :: Parser Builder
parenComp = do spaces
               is '('
               x <- exprComp
               is ')'
               pure x

-- Creates a Builder expression that checks whether a number is 0.
isZero :: Builder
isZero = lam 'n' (term 'n' `ap` lam 'x' (boolToLam False) `ap` boolToLam True)

-- Parser to parse the operator "<=".
lessOrEqualParser :: Parser (Builder -> Builder -> Builder)
lessOrEqualParser = do literal "<="
                       pure lessOrEqualBuilder

-- Creates a Builder expression for the operator "<=".
lessOrEqualBuilder :: Builder -> Builder -> Builder
lessOrEqualBuilder = ap . ap (lam 'm' (lam 'n' (isZero `ap` minusBuilder (term 'm') (term 'n'))))

-- Parser to parse the operator "==" for numbers.
equalIntParser :: Parser (Builder -> Builder -> Builder)
equalIntParser = do literal "=="
                    pure equalIntBuilder

-- Creates a Builder expression for the operator "==" for numbers.
equalIntBuilder :: Builder -> Builder -> Builder
equalIntBuilder = ap . ap (lam 'm' (lam 'n'
    (andBuilder (lessOrEqualBuilder (term 'm') (term 'n')) (lessOrEqualBuilder (term 'n') (term 'm')))))

-- Parser to parse the operator "==" for boolean values.
equalBoolParser :: Parser (Builder -> Builder -> Builder)
equalBoolParser = do literal "=="
                     pure xnor

-- Creates a Builder expression for the operator "==" for boolean values.
-- XNOR church encoding modified from XOR church encoding. 
-- Referenced from https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans
xnor :: Builder -> Builder -> Builder
xnor m n = notBuilder `ap` (lam 'm' (lam 'n'
    (ifBuilder `ap` term 'm' `ap` (notBuilder `ap` term 'n') `ap` term 'n')) `ap` m `ap` n)

-- Parser to parse the operator "!=".
notEqualParser :: Parser (Builder -> Builder -> Builder)
notEqualParser = do literal "!="
                    pure notEqualBuilder

-- Creates a Builder expression for the operator "!=".
notEqualBuilder :: Builder -> Builder -> Builder
notEqualBuilder m n = notBuilder `ap` equalIntBuilder m n

-- Parser to parse the operator ">".
greaterParser :: Parser (Builder -> Builder -> Builder)
greaterParser = do literal ">"
                   pure greaterBuilder

-- Creates a Builder expression for the operator ">".
greaterBuilder :: Builder -> Builder -> Builder
greaterBuilder m n = notBuilder `ap` lessOrEqualBuilder m n

-- Parser to parse the operator "<".
lessParser :: Parser (Builder -> Builder -> Builder)
lessParser = do literal "<"
                pure lessBuilder

-- Creates a Builder expression for the operator "<".
lessBuilder :: Builder -> Builder -> Builder
lessBuilder m n = greaterBuilder n m

-- Parser to parse the operator ">=".
greaterOrEqualParser :: Parser (Builder -> Builder -> Builder)
greaterOrEqualParser = do literal ">="
                          pure greaterOrEqualBuilder

-- Creates a Builder expression for the operator ">=".
greaterOrEqualBuilder :: Builder -> Builder -> Builder
greaterOrEqualBuilder m n = notBuilder `ap` lessBuilder m n

-- Parser that parses a complex calculation expression.
complexCalcP :: Parser Lambda
complexCalcP = build <$> exprCompAnd

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
-- Result >< (\htcn.ch(tcn))(\t_.t)\cn.n
--
-- >>> parse listP "[0, 0]"
-- Result >< (\htcn.ch(tcn))(\fx.x)((\htcn.ch(tcn))(\fx.x)\cn.n)
--
-- >>> parse listP "[0, 0"
-- UnexpectedEof

-- Creates a Builder expression for an empty list.
nullBuilder :: Builder
nullBuilder = lam 'c' (lam 'n' (term 'n'))

-- Creates a Builder expression for the operator "isNull".
isNullBuilder :: Builder
isNullBuilder = lam 'l' (term 'l' `ap` lam 'h' (lam 't' (boolToLam False)) `ap` boolToLam True)

-- Creates a Builder expression for the operator "cons".
consBuilder :: Builder
consBuilder = lam 'h' (lam 't' (lam 'c' (lam 'n'
    (term 'c' `ap` term 'h' `ap` (term 't' `ap` term 'c' `ap` term 'n')))))

-- Creates a Builder expression for the operator "head".
headBuilder :: Builder
headBuilder = lam 'l' (term 'l' `ap` lam 'h' (lam 't' (term 'h')) `ap` boolToLam False)

-- Creates a Builder expression for the operator "tail".
tailBuilder :: Builder
tailBuilder = lam 'l' (lam 'c' (lam 'n' (term 'l' `ap` lam 'h' (lam 't' (lam 'g'
    (term 'g' `ap` term 'h' `ap` (term 't' `ap` term 'c')))) `ap` lam 't' (term 'n') `ap`
    lam 'h' (lam 't' (term 't')))))

-- Parser that creates a Builder expression for the list expression.
exprList :: Parser Builder
exprList = chainr1 atomList (pure ap)

-- Parser that combines various parsers together to process the characters in the list expression.
atomList :: Parser Builder
atomList = (spaces >> is '[' >> is ']' >> pure nullBuilder)
           ||| listComma ||| openB ||| closeB ||| exprComp ||| exprLogic ||| exprArith

-- Parser that parses the character '['.
openB :: Parser Builder
openB = do spaces
           is '['
           x <- atomList
           y <- exprList
           pure $ consBuilder `ap` x `ap` y

-- Parser that parses the character ','.
listComma :: Parser Builder
listComma = do spaces
               is ','
               spaces
               x <- atomList
               y <- exprList
               pure $ consBuilder `ap` x `ap` y

-- Parser that parses the character ']'.
closeB :: Parser Builder
closeB = do spaces
            is ']'
            pure nullBuilder

-- Parser that parses a list expression.
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

-- Parser that creates a Builder expression for the list operator or the list.
exprListOp :: Parser Builder
exprListOp = chainr1 atomListOp (pure ap)

-- Parser that combines various parsers together to process the characters in the 
-- list operator or the list.
atomListOp :: Parser Builder
atomListOp = exprList ||| tailParser ||| headParser ||| isNullParser ||| consParser

-- Parser to parse the operator "head".
headParser :: Parser Builder
headParser = do literal "head"
                spaces1
                pure headBuilder

-- Parser to parse the operator "tail".
tailParser :: Parser Builder
tailParser = do literal "rest"
                spaces1
                pure tailBuilder
             ||| do literal "tail"
                    spaces1
                    pure tailBuilder

-- Parser to parse the operator "isNull".
isNullParser :: Parser Builder
isNullParser = do literal "isNull"
                  spaces1
                  pure isNullBuilder

-- Parser to parse the operator "cons".
consParser :: Parser Builder
consParser = do literal "cons"
                spaces1
                pure consBuilder

-- Parser that parses a list operator or a list.
listOpP :: Parser Lambda
listOpP = build <$> exprListOp

-- | Exercise 2

-- | Implement your function(s) of choice below!

-- Implementation details of these functions will be discussed in the report.

-- Factorial Function
-- Church encoding of factorial adapted from https://groups.seas.harvard.edu/courses/cs152/2016sp/lectures/lec08-encodings.pdf

-- Handle repeated chains of operators of unknown length that are left associative
-- that takes in a unary function rather than a binary function as chainl1 does.
chainl2 :: Parser a -> Parser (a->a) -> Parser a
chainl2 p op = p >>= rest
    where rest a = (do
                      f <- op
                      rest (f a)
                   ) ||| pure a

-- Parser that parses the factorial of a number.
factP :: Parser Lambda
factP = build <$> chainl2 naturalNo factParser

-- Creates a Builder expression of the number 0.
zero :: Builder
zero = lam 'f' (lam 'x' (term 'x'))

-- Creates a Builder expression of the number 1.
one :: Builder
one = lam 'f' (lam 'x' (term 'f' `ap` term 'x'))

-- Parser to parse the operator "!".
factParser :: Parser (Builder -> Builder)
factParser = do operator '!'
                pure factBuilder

-- Creates a Builder expression for the operator "!".
factBuilder :: Builder -> Builder
factBuilder = ap (ap fact' fact')

-- Auxiliary function for to achieve recursion for factorial.
fact' :: Builder
fact' = lam 'f' (lam 'n' (ifBuilder `ap` (isZero `ap` term 'n') `ap` one `ap`
    (multBuilder `ap` term 'n' `ap` (term 'f' `ap` term 'f' `ap` (pred1 `ap` term 'n')))))


-- Negative Numbers
-- Church encoding of negative numbers adapted from https://en.wikipedia.org/wiki/Church_encoding#Predicates

-- Creates a Builder expression for a pair of items.
pair :: Builder
pair = lam 'x' (lam 'y' (lam 'z' (term 'z' `ap` term 'x' `ap` term 'y')))

-- Creates a Builder expression that returns the first item in the pair.
first :: Builder
first = lam 'p' (term 'p' `ap` lam 'x' (lam 'y' (term 'x')))

-- Creates a Builder expression that returns the second item in the pair.
second :: Builder
second = lam 'p' (term 'p' `ap` lam 'x' (lam 'y' (term 'y')))

-- Creates a Builder expression that converts a natural number into a signed number.
convert :: Builder
convert = lam 'x' (pair `ap` term 'x' `ap` zero)

-- Creates a Builder expression of a negative number.
negativeNo :: Parser Builder
negativeNo = do operator '-'
                x <- munch1 isDigit
                let y = read x
                pure $ intToLam y

-- Creates a Builder expression that negates a signed number.
neg :: Builder
neg = lam 'x' (pair `ap` (second `ap` term 'x') `ap` (first `ap` term 'x'))

-- Parser that parses a negative number.
negP :: Parser Lambda
negP = do x <- negativeNo
          pure . build $ neg `ap` (convert `ap` x)
