
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.LCS
-- Copyright   :  (c) Ian Lynagh 2005, 2008
-- License     :  BSD or GPL v2
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  provisional
-- Portability :  non-portable (HuntSzymanski implementation is non-portable)
--
-- Provides a function lcs that takes two lists and returns a longest common
-- sublist. For example, lcs "abcd" "acbd" is either "abd" or "acd".
-----------------------------------------------------------------------------

module Data.List.LCS (lcs) where

import Data.List.LCS.HuntSzymanski (lcs)

