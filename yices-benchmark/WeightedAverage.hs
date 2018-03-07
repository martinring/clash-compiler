module WeightedAverage where

import CLaSH.Prelude

type Configuration = (Unsigned 16, Unsigned 16, Unsigned 16, Vec 16 (Unsigned 16))

configurationT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

configuration :: Signal (Bool, Configuration) -> Signal Configuration
configuration = mealy configurationT (63,191,127,replicate SNat 0)

type ControllerState = (Bool,Unsigned 16)

controllerT :: ControllerState -> (Configuration, Vec 16 (Unsigned 16)) -> (ControllerState,Bool)
controllerT (switchState,cnt) ((l_low,l_high,delay,weights),sensors)
  | e > l_high               = ((True,cntn),True)
  | e < l_low && cnt > delay = ((False,cntn),False)
  | otherwise                     = ((switchState,cntn),switchState)
    where cntn    = if switchState then cnt + 1 else 0
          wsum    = sum weights
          esum    = sum $ zipWith (*) sensors weights
          e       = resize $ esum `div` wsum

controller :: Signal (Configuration, Vec 16 (Unsigned 16)) -> Signal Bool
controller = mealy controllerT (False,0)

configuredController :: Signal (Bool, Configuration, Vec 16 (Unsigned 16)) -> Signal Bool
configuredController input = controller (bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configuration (bundle (enable,config))

topEntity = configuredController