module CSS.Schema (
    asClass, fromClass
  , ClassName(..)
  ) where

import Prelude
import CSS.Selector
import Data.Monoid
import Data.String  (trim)
import CSS.String   (fromString)
-------------------CLASS NAMES-------------------

-- | ClassName Type (a string with some unique type class instances)
newtype ClassName = CLS String

-- | constructor
asClass :: String -> ClassName
asClass = CLS

-- | Transform a Classname into a CSS.Selector
fromClass :: ClassName -> Selector
fromClass = fromString <<< ("." <> ) <<< show

-- | Semigroup instance which trims the strings and inserts a space between them
instance classNameSemiGroup :: Semigroup ClassName where
  append (CLS s1) (CLS s2) = CLS ( trim s1 `sp` trim s2 )

instance classNameMonoid :: Monoid ClassName where
  mempty = CLS ""

instance showClassName :: Show ClassName where
  show (CLS s) = s

sp :: String -> String -> String
sp s s' = s <> " " <> s'
