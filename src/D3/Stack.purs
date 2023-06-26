module D3.Stack
  ( StackData
  , StackResult
  , _bindDataLoopStack
  , _stackData
  , _stackData0
  , _stackData1
  , _stackResultData
  , _stackResultIndex
  , _stackResultKey
  ) where

import D3.Types (Selection)
import Effect (Effect)

type StackData datum
  = { data :: datum
    }

foreign import data StackResult :: Type -> Type -> Type

foreign import _bindDataLoopStack ::
  forall key datum.
  Selection (StackResult key datum) ->
  Effect (Selection (StackData datum))

foreign import _stackResultData :: forall k d. StackResult k d -> Array (StackData d)

foreign import _stackResultIndex :: forall k d. StackResult k d -> Int

foreign import _stackResultKey :: forall k d. StackResult k d -> k

foreign import _stackData0 :: forall d. StackData d -> Number

foreign import _stackData1 :: forall d. StackData d -> Number

foreign import _stackData ::
  forall key datum. Array key -> Array datum -> Effect (Array (StackResult key datum))
