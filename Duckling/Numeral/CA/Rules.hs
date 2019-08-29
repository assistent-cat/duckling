-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Numeral.CA.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral

ruleNumeralsPrefixWithNegativeOrMinus :: Rule
ruleNumeralsPrefixWithNegativeOrMinus = Rule
  { name = "numbers prefix with -, negative or minus"
  , pattern =
    [ regex "-|menys"
    , Predicate isPositive
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        double $ v * (- 1)
      _ -> Nothing
  }

ruleDecimalWithThousandsSeparator :: Rule
ruleDecimalWithThousandsSeparator = Rule
  { name = "decimal with thousands separator"
  , pattern =
    [ regex "(\\d+(\\.\\d\\d\\d)+,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> let fmt = Text.replace "," "." $ Text.replace "." Text.empty match
        in parseDouble fmt >>= double
      _ -> Nothing
  }

ruleDecimalNumeral :: Rule
ruleDecimalNumeral = Rule
  { name = "decimal number"
  , pattern =
    [ regex "(\\d*,\\d+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDecimal False match
      _ -> Nothing
  }

byTensMap :: HashMap.HashMap Text.Text Integer
byTensMap = HashMap.fromList
  [ ( "vint" , 20 )
  , ( "trenta" , 30 )
  , ( "quaranta" , 40 )
  , ( "cinquanta" , 50 )
  , ( "seixanta" , 60 )
  , ( "setanta" , 70 )
  , ( "vuitanta" , 80 )
  , ( "noranta" , 90 )
  ]

ruleTens :: Rule
ruleTens = Rule
  { name = "number (20..90)"
  , pattern =
    [ regex "(vint|trenta|quaranta|cinquanta|seixanta|setanta|vuitanta|noranta)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) byTensMap >>= integer
      _ -> Nothing
  }

zeroToNineteenMap :: HashMap.HashMap Text.Text Integer
zeroToNineteenMap = HashMap.fromList
  [ ( "zero" , 0 )
  , ( "u" , 1 )
  , ( "un" , 1 )
  , ( "una" , 1 )
  , ( "dos" , 2 )
  , ( "dues" , 2 )
  , ( "tres" , 3 )
  , ( "quatre" , 4 )
  , ( "cinc" , 5 )
  , ( "sis" , 6 )
  , ( "set" , 7 )
  , ( "vuit" , 8 )
  , ( "nou" , 9 )
  , ( "deu" , 10 )
  , ( "onze" , 11 )
  , ( "dotze" , 12 )
  , ( "tretze" , 13 )
  , ( "catorze" , 14 )
  , ( "quinze" , 15 )
  , ( "setze" , 16 )
  , ( "disset" , 17 )
  , ( "divuit" , 18 )
  , ( "dinou" , 19 )
  ]

ruleToNineteen :: Rule
ruleToNineteen = Rule
  { name = "number (0..19)"
  , pattern =
    [ regex "(zero|u(na?)?|d(o|ue)s|tres|quatre|cinc|sis|set(ze)?|vuit|nou|deu|onze|dotze|tretze|catorze|quinze|disset|divuit|dinou)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        HashMap.lookup (Text.toLower match) zeroToNineteenMap >>= integer
      _ -> Nothing
  }

ruleTwenties :: Rule
ruleTwenties = Rule
  { name = "number (21..29)"
  , pattern =
    [ numberWith TNumeral.value (== 20)
    , regex "-i-"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
    (Token Numeral NumeralData{TNumeral.value = tens}:
     _:
     Token Numeral NumeralData{TNumeral.value = units}:
     _) -> double $ tens + units
    _ -> Nothing
  }

ruleCompositeTens :: Rule
ruleCompositeTens = Rule
  { name = "number (31..99)"
  , pattern =
    [ oneOf [30..90]
    , regex "(\\-|\\s)"
    , numberBetween 1 10
    ]
  , prod = \tokens -> case tokens of
    (Token Numeral NumeralData{TNumeral.value = tens}:
      _:
      Token Numeral NumeralData{TNumeral.value = units}:
      _) -> double $ tens + units
    _ -> Nothing
  }

ruleNumeralsSuffixesKMG :: Rule
ruleNumeralsSuffixesKMG = Rule
  { name = "numbers suffixes (K, M, G)"
  , pattern =
    [ dimension Numeral
    , regex "([kmg])(?=[\\W\\$€]|$)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
          "k" -> double $ v * 1e3
          "m" -> double $ v * 1e6
          "g" -> double $ v * 1e9
          _ -> Nothing
      _ -> Nothing
  }

rulePowersOfTen :: Rule
rulePowersOfTen = Rule
  { name = "powers of tens"
  , pattern = [regex "(\\-?cente?s?|((m|b)il(ió|io(ns)?)?))"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match : _)) : _) ->
        case Text.toLower match of
          "cent" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "cents" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "centes" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "-cents" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "-centes" -> double 1e2 >>= withGrain 2 >>= withMultipliable
          "mil" -> double 1e3 >>= withGrain 3 >>= withMultipliable
          "milio" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "milió" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "milions" -> double 1e6 >>= withGrain 6 >>= withMultipliable
          "bilio" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          "bilió" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          "bilions" -> double 1e9 >>= withGrain 9 >>= withMultipliable
          _ -> Nothing
      _ -> Nothing
  }

ruleNumeralDotNumeral :: Rule
ruleNumeralDotNumeral = Rule
  { name = "number dot number"
  , pattern =
    [ dimension Numeral
    , regex "coma"
    , Predicate $ not . hasGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v1}:
       _:
       Token Numeral NumeralData{TNumeral.value = v2}:
       _) -> double $ v1 + decimalsToDouble v2
      _ -> Nothing
  }

ruleIntegerWithThousandsSeparator :: Rule
ruleIntegerWithThousandsSeparator = Rule
  { name = "integer with thousands separator ."
  , pattern =
    [ regex "(\\d{1,3}(\\.\\d\\d\\d){1,5})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        parseDouble (Text.replace "." Text.empty match) >>= double
      _ -> Nothing
  }

ruleSum :: Rule
ruleSum = Rule
  { name = "intersect 2 numbers"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isPositive]
    , Predicate $ and . sequence [not . isMultipliable, isPositive]
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
        Token Numeral NumeralData{TNumeral.value = val2}:
        _) | (10 ** fromIntegral g) > val2 -> double $ val1 + val2
      _ -> Nothing
  }

ruleMultiply :: Rule
ruleMultiply = Rule
  { name = "compose by multiplication"
  , pattern =
    [ dimension Numeral
    , Predicate isMultipliable
    ]
  , prod = \tokens -> case tokens of
      (token1:token2:_) -> multiply token1 token2
      _ -> Nothing
  }
  
rules :: [Rule]
rules =
  [ ruleCompositeTens
  , ruleDecimalNumeral
  , ruleDecimalWithThousandsSeparator
  , ruleIntegerWithThousandsSeparator
  , ruleMultiply
  , ruleNumeralDotNumeral
  , ruleNumeralsPrefixWithNegativeOrMinus
  , ruleNumeralsSuffixesKMG
  , rulePowersOfTen
  , ruleSum
  , ruleTens
  , ruleToNineteen
  , ruleTwenties
  ]
