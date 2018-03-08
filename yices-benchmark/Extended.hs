module Extended where

import CLaSH.Prelude

type Configuration = (Unsigned 16, Unsigned 16, Unsigned 16, Vec 16 (Vec 16 (Unsigned 16)))

configurationT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

configuration :: Signal (Bool, Configuration) -> Signal Configuration
configuration = mealy configurationT (63,191,127,replicate SNat (replicate SNat 0))

type ControllerState = Vec 16 (Bool, Unsigned 16)

controllerT :: ControllerState -> (Configuration, Vec 16 (Unsigned 16)) -> (ControllerState, Vec 16 Bool)
controllerT state ((l_low,l_high,delay,lweights),sensors) = (state',map fst state') where
  state' = zipWith controllerT' state lweights
  controllerT' (switchState, cnt) weights
    | e > l_high               = (True,cntn)
    | e < l_low && cnt > delay = (False,cntn)
    | otherwise                = (switchState,cntn)
      where cntn    = if switchState then cnt + 1 else 0
            wsum    = sum weights
            esum    = sum $ zipWith (*) sensors weights
            e       = resize $ esum `div` wsum

controller :: Signal (Configuration, Vec 16 (Unsigned 16)) -> Signal (Vec 16 Bool)
controller = mealy controllerT $ replicate SNat (False,0)

configuredController :: Signal (Bool, Configuration, Vec 16 (Unsigned 16)) -> Signal (Vec 16 Bool)
configuredController input = controller (bundle (cfgOut,sensors))
  where (enable,config,sensors) = unbundle input
        cfgOut = configuration (bundle (enable,config))

topEntity = configuredController