module HW03Tests where

import HW03
import Testing
import Text.Show.Functions()

one :: State
one = extend empty "1" 1

two :: State
two = extend one "2" 2

three :: State
three = extend two "3" 3

extendEmptyTests :: [Test]
extendEmptyTests = [ testF4 "extends test" extend
                     [ (empty, "1", 1, "1", 1)
                     , (empty, "1", 1, "2", 0)
                     ]
                   ]

extendTests :: [Test]
extendTests = [ testF1 "existing state extends test" three
                [ ("1", 1)
                , ("2", 2)
                , ("3", 3)
                ]
              ]