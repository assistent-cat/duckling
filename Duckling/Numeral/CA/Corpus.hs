-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CA.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Numeral.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (NumeralValue 1)
             [ "1"
             , "u"
             , "un"
             , "una"
             ]
  , examples (NumeralValue 11)
             [ "onze"
             ]
  , examples (NumeralValue 16)
             [ "setze"
             ]
  , examples (NumeralValue 21)
             [ "vint-i-u"
             , "vint-i-un"
             , "vint-i-una"
             ]
  , examples (NumeralValue 22)
             [ "vint-i-dos"
             , "vint-i-dues"
             ]
  , examples (NumeralValue 23)
             [ "vint-i-tres"
             ]
  , examples (NumeralValue 70)
             [ "setanta"
             ]
  , examples (NumeralValue 78)
             [ "setanta-vuit",
               "setanta vuit"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trenta-tres"
             , "trenta tres"
             ]
  , examples (NumeralValue 100)
             [ "cent"
             ]
  , examples (NumeralValue 1.1)
             [ "1,1"
             , "1,10"
             , "01,10"
             ]
  , examples (NumeralValue 0.77)
             [ "0,77"
             , ",77"
             ]
  , examples (NumeralValue 3455.77)
             [ "3.455,77"
             , "tres mil quatre-cents cinquanta-cinc coma setanta-set"
             ]
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             , "cent mil"
             ]
  , examples (NumeralValue 300)
             [ "tres-cents"
             , "tres cents"
             ]
  , examples (NumeralValue 2000)
             [ "dos mil"
             , "dues mil"
             ]
  , examples (NumeralValue 4000)
             [ "quatre mil"
             , "0004000"
             ]
  , examples (NumeralValue 243)
             [ "243"
             , "dos-cents quaranta-tres"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             , "tres milions"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             , "un milió dos-cents mil"
             , "un milio dos cents mil"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "menys 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             , "menys un milió dos cents mil"
             , "menys un milio dos-cents mil"
             ]
  , examples (NumeralValue 1.5)
             [ "1 coma cinc"
             , "u coma cinc"
             , "una coma cinc"
             , "1,5"
             ]
  , examples (NumeralValue 999999999999)
             [ "nou-cents noranta-nou bilions nou-cents noranta-nou milions nou-cents noranta-nou mil nou-cents noranta-nou"]
  ]
