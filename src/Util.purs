module Util where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.Bus as Bus
import Halogen.HTML.Core as HH
import Halogen.HTML.Properties as HP
import Halogen as H
import Halogen.Query.EventSource as ES

classes :: forall r i. Array String -> HP.IProp ( class :: String | r ) i
classes = HP.classes <<< map HH.ClassName

type OpaqueSlot slot
  = forall query. H.Slot query Void slot

busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource
    $ \emitter -> do
        fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
        pure $ ES.Finalizer $ killFiber (error "Event source closed") fiber
