module RegexParsing (parseRegex) where

import qualified Text.Parsec as Parsec
import qualified StateMachines
import Data.Functor ((<$>))
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

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
single = ordChar <|> quotedChar <|> dot
         
metacharacters :: String
metacharacters = "^.[$()|*+?{\\"

ordChar :: Parser StateMachines.StateMachine
ordChar = StateMachines.specificChar <$> Parsec.noneOf metacharacters

quotedChar :: Parser StateMachines.StateMachine
quotedChar = Parsec.char '\\' >>
             (StateMachines.specificChar <$> Parsec.oneOf metacharacters)

dot :: Parser StateMachines.StateMachine
dot = Parsec.char '.' >> return StateMachines.anyChar

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
