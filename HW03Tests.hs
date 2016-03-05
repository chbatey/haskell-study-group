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

inOne :: State
inOne = extend empty "In" 1

inThree :: State
inThree = extend empty "In" 3

ifTests :: [Test]
ifTests = [ testF3 "test simples" run
                    [ (inOne, simples, "In", 2)
                    , (inThree, simples, "In", 3)
                    , (inThree, simples, "In2", 1)
                    ]
                  ]

whileTests :: [Test]
whileTests = [testF3 "test while" run
                [ (inOne, whiles, "In", 5)
                , (inOne, whiles, "X", 4)
                ]
             ]

simpleForsTests :: [Test]
simpleForsTests = [testF3 "test a simple for" run
                    [ (empty, simpleFors, "Out", 3)
                    , (empty, simpleFors, "Out2", 2)
                    ]
                 ]


forTests :: [Test]
forTests =  [testF3 "test for" run
                [ (inThree, fors, "Out", 6 )
                ]
            ]

facTests :: [Test]
facTests = [testF3 "test fac" run
            [ (inOne, factorial, "Out", 1)
            ]
           ]