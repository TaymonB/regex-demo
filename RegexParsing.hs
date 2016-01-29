module RegexParsing (parseRegex) where

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Text.Parsec as Parsec
import qualified StateMachines
import Data.Functor ((<$>))
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Debug.Trace

parseRegex :: String -> Either Parsec.ParseError StateMachines.StateMachine
parseRegex = Parsec.parse outerRegex ""

outerRegex :: Parser StateMachines.StateMachine
outerRegex = do
  machine <- regex
  Parsec.eof
  return machine

regex :: Parser StateMachines.StateMachine
regex = foldl1 StateMachines.alt <$> Parsec.sepBy1 branch (Parsec.char '|')

branch :: Parser StateMachines.StateMachine
branch = foldl1 StateMachines.cat <$> Parsec.many1 expression

expression :: Parser StateMachines.StateMachine
expression = do
  base <- single <|> Parsec.between (Parsec.char '(') (Parsec.char ')') regex
  duplications <- Parsec.many duplication
  return $ foldl (flip ($)) base duplications

single :: Parser StateMachines.StateMachine
single = ordChar <|> quotedChar <|> dot <|> bracket
         
metacharacters :: String
metacharacters = "^.[$()|*+?{\\"

ordChar :: Parser StateMachines.StateMachine
ordChar = StateMachines.specificChar <$> Parsec.noneOf metacharacters

quotedChar :: Parser StateMachines.StateMachine
quotedChar = Parsec.char '\\' >>
             (StateMachines.specificChar <$> Parsec.oneOf metacharacters)

dot :: Parser StateMachines.StateMachine
dot = Parsec.char '.' >> return StateMachines.anyChar

bracket :: Parser StateMachines.StateMachine
bracket = do
  Parsec.char '['
  invert <- Parsec.optionMaybe $ Parsec.char '^'
  rbracket <- Parsec.optionMaybe $ Parsec.char ']'
  startHyphen <- Parsec.optionMaybe $ Parsec.char '-'
  charsets <- Parsec.many $ charClass <|> bracketCharset
  endHyphen <- Parsec.optionMaybe $ Parsec.char '-'
  --trace (show rbracket) $ trace (show startHyphen) $ trace (show endHyphen) $ return ()
  Parsec.char ']'
  let withHyphen =
        if Maybe.isJust startHyphen || Maybe.isJust endHyphen
           then (== '-'):charsets else charsets
  let predicate char =
        (any . flip ($)) char $
        if Maybe.isJust rbracket then (== ']'):withHyphen else withHyphen
  return $ StateMachines.singleCharMatcher $
    if Maybe.isJust invert then not . predicate else predicate

bracketCharset :: Parser StateMachines.TransitionPredicate
bracketCharset = do
  startChar <- bracketChar
  range <- Parsec.try $ Parsec.optionMaybe $ do
    Parsec.char '-'
    bracketChar
  return $ case range of
    Nothing -> (== startChar)
    Just endChar ->
      let inRange char = char >= startChar && char <= endChar in inRange

bracketChar :: Parser Char
bracketChar = Parsec.noneOf "[]-\\" <|> (Parsec.char '\\' >> Parsec.oneOf "[]-\\")

charClass :: Parser StateMachines.TransitionPredicate
charClass = do
  Parsec.string "[:"
  className <- Parsec.many1 Parsec.lower
  Parsec.string ":]"
  case className of
    "alnum" -> return $ inUnicodeCategory $ letterCategories ++ numberCategories
    "alpha" -> return Char.isLetter
    "ascii" -> return Char.isAscii
    "blank" -> return $
               let blank char =
                     inUnicodeCategory [Char.Space] char || char == '\t'
               in blank
    "cntrl" -> return $ inUnicodeCategory [Char.Control]
    "digit" -> return $ inUnicodeCategory numberCategories
    "graph" -> return $ not . inUnicodeCategory
               (separatorCategories ++ otherCategories)
    "lower" -> return Char.isLower
    "print" -> return $ not . inUnicodeCategory otherCategories
    "punct" -> return Char.isPunctuation
    "space" -> return $
               let space char =
                     Char.isSeparator char || elem char "\t\r\n\v\f"
               in space
    "upper" -> return $ inUnicodeCategory [Char.UppercaseLetter]
    "word" -> return $ inUnicodeCategory $
              Char.ConnectorPunctuation:letterCategories ++ numberCategories
    "xdigit" -> return Char.isHexDigit
    _ -> fail $ "no such character class: " ++ className

inUnicodeCategory :: [Char.GeneralCategory] -> StateMachines.TransitionPredicate
inUnicodeCategory categories char = elem (Char.generalCategory char) categories

letterCategories :: [Char.GeneralCategory]
letterCategories =
  [Char.UppercaseLetter
  ,Char.LowercaseLetter
  ,Char.TitlecaseLetter
  ,Char.ModifierLetter
  ,Char.OtherLetter]

numberCategories :: [Char.GeneralCategory]
numberCategories = [Char.DecimalNumber]

separatorCategories :: [Char.GeneralCategory]
separatorCategories =
  [Char.Space
  ,Char.LineSeparator
  ,Char.ParagraphSeparator]

otherCategories :: [Char.GeneralCategory]
otherCategories =
  [Char.Control
  ,Char.Format
  ,Char.Surrogate
  ,Char.PrivateUse
  ,Char.NotAssigned]

duplication :: Parser (StateMachines.StateMachine -> StateMachines.StateMachine)
duplication = star <|> plus <|> question <|> curly

star :: Parser (StateMachines.StateMachine -> StateMachines.StateMachine)
star = Parsec.char '*' >> return StateMachines.kleene

plus :: Parser (StateMachines.StateMachine -> StateMachines.StateMachine)
plus = Parsec.char '+' >> return StateMachines.oneOrMore

question :: Parser (StateMachines.StateMachine -> StateMachines.StateMachine)
question = Parsec.char '?' >> return StateMachines.optional

curly :: Parser (StateMachines.StateMachine -> StateMachines.StateMachine)
curly = do
  Parsec.char '{'
  lowerCount <- nat
  upper <- Parsec.optionMaybe $ Parsec.char ',' >> Parsec.optionMaybe nat
  Parsec.char '}'
  return $ case upper of
             Nothing -> StateMachines.repeatFixed lowerCount
             Just Nothing -> StateMachines.repeatAtLeast lowerCount
             Just (Just upperCount) ->
                 StateMachines.repeatBounded lowerCount upperCount

nat :: Parser Int
nat = read <$> Parsec.many1 Parsec.digit
