module Meeshkan.Variant where

import Prelude

import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep)
import Unsafe.Coerce (unsafeCoerce)

foreign import unsafeSetViaVariant :: forall a b. VariantRep a -> { | b } -> { | b }

setViaVariant :: forall row. Variant row -> { | row } -> { | row }
setViaVariant = unsafeSetViaVariant <<< coerceV
  where
  coerceV :: forall a. Variant row -> VariantRep a
  coerceV = unsafeCoerce