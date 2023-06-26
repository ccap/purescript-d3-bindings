module D3.Properties
  ( c
  , cMe
  , d
  , dMe
  , di
  , diMe
  , overPropFn
  ) where

import Prelude
import D3.Types as Types

-- PropFn helpers 
c :: forall datum a. a -> Types.PropFn datum a
c = Types.Constant

d :: forall datum a. Types.DatumFn datum a -> Types.PropFn datum a
d = Types.Datum

di :: forall datum a. Types.IndexedDatumFn datum a -> Types.PropFn datum a
di = Types.IndexedDatum

overPropFn :: forall datum a b. Types.PropFn datum a -> (a -> b) -> Types.PropFn datum b
overPropFn propFn fn = case propFn of
  Types.Constant value -> Types.Constant $ fn value
  Types.Datum toValue -> Types.Datum \datum -> fn $ toValue datum
  Types.IndexedDatum toValue -> Types.IndexedDatum \datum index -> fn $ toValue datum index

-- PropFnMouseEvent helpers 
cMe :: forall datum. Types.MouseEventConstantFn -> Types.PropFnMouseEvent datum
cMe = Types.MouseEventConstant

dMe :: forall datum. Types.MouseEventDatumFn datum -> Types.PropFnMouseEvent datum
dMe = Types.MouseEventDatum

diMe :: forall datum. Types.MouseEventIndexedDatumFn datum -> Types.PropFnMouseEvent datum
diMe = Types.MouseEventIndexedDatum
