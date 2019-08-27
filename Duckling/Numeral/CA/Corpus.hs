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
corpus = (testContext {locale = makeLocale ES Nothing}, testOptions, allExamples)

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
             [ "setanta-vuit"
             ]
  , examples (NumeralValue 80)
             [ "vuitanta"
             ]
  , examples (NumeralValue 33)
             [ "33"
             , "trenta-tres"
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
  , examples (NumeralValue 100000)
             [ "100.000"
             , "100000"
             , "100K"
             , "100k"
             ]
  , examples (NumeralValue 300)
             [ "tres-cents"
             ]
  , examples (NumeralValue 243)
             [ "243"
             ]
  , examples (NumeralValue 3000000)
             [ "3M"
             , "3000K"
             , "3000000"
             , "3.000.000"
             ]
  , examples (NumeralValue 1200000)
             [ "1.200.000"
             , "1200000"
             , "1,2M"
             , "1200K"
             , ",0012G"
             ]
  , examples (NumeralValue (-1200000))
             [ "- 1.200.000"
             , "-1200000"
             , "menys 1.200.000"
             , "-1,2M"
             , "-1200K"
             , "-,0012G"
             ]
  , examples (NumeralValue 1.5)
             [ "1 coma cinc"
             , "una coma cinc"
             , "1,5"
             ]
  ]
