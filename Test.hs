{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen
import qualified StateMachines
import qualified Control.Monad.Omega as Omega
import Data.Functor ((<$>))
import Test.QuickCheck ((==>), quickCheckAll)

data Matcher = Matcher StateMachines.StateMachine [String] deriving Show

interleave :: [a] -> [a] -> [a]
interleave list1 [] = list1
interleave [] list2 = []
interleave (first1:rest1) (first2:rest2) = first1:first2:interleave rest1 rest2

getAllNLists :: [a] -> Int -> [[a]]
getAllNLists list n =
    case n of
      0 -> [[]]
      _ -> let previousNLists = getAllNLists list (n - 1)
               allPreviousPlus elem = map (elem:) previousNLists in
           Omega.diagonal $ map allPreviousPlus list

instance QuickCheck.Arbitrary Matcher where
    arbitrary =
        Gen.frequency
               [(4,do
                   char <- QuickCheck.arbitrary
                   return $ Matcher (StateMachines.specificChar char) [[char]])
               ,(2,return $ Matcher StateMachines.anyChar
                      $ map (:[]) [minBound..maxBound])
               ,(1,do
                   (Matcher machine1 strs1) <- QuickCheck.arbitrary
                   (Matcher machine2 strs2) <- QuickCheck.arbitrary
                   return $ Matcher (StateMachines.alt machine1 machine2)
                              $ interleave strs1 strs2)
               ,(1,do
                   (Matcher machine strs) <- QuickCheck.arbitrary
                   return $ Matcher (StateMachines.kleene machine)
                              $ map concat $ Omega.diagonal
                                        $ map (getAllNLists strs) [0..])]

prop_matcherWorks (Matcher machine strs) =
    all (StateMachines.matches machine) $ take 500 strs

instance QuickCheck.Arbitrary StateMachines.StateMachine where
    arbitrary = do
      Matcher machine _ <- QuickCheck.arbitrary
      return machine

prop_singleCharMatch char =
    StateMachines.matches (StateMachines.specificChar char) [char]

prop_singleCharOnlyMatch char1 char2 =
    char1 /= char2 ==>
          not $ StateMachines.matches (StateMachines.specificChar char1) [char2]

prop_dotMatch char =
    StateMachines.matches StateMachines.anyChar [char]

--prop_consMatch machine1 machine2 str1 str2 =
--    (StateMachines.matches machine1 str1 && StateMachines.matches machine2 str2)
--    ==> StateMachines.matches (StateMachines.cat machine1 machine2) (str1 ++ str2)
                 
return []
main = $quickCheckAll
