module Average where

import CLaSH.Prelude

type Configuration = (Unsigned 16, Unsigned 16, Unsigned 16, Vec 16 Bool)

configurationT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

configuration :: Signal (Bool, Configuration) -> Signal Configuration
configuration = mealy configurationT (63,191,127,replicate SNat False)

type ControllerState = (Bool,Unsigned 16)

controllerT :: ControllerState -> (Configuration, Unsigned 24) -> (ControllerState,Bool)
controllerT (switchState,cnt) ((l_low,l_high,delay,active),sensor)
  | e > l_high               = ((True,cntn),True)
  | e < l_low && cnt > delay = ((False,cntn),False)
  | otherwise                = ((switchState,cntn),switchState)
    where cntn    = if switchState then cnt + 1 else 0
          nactive = foldl (\acc b -> if b then acc + 1 else acc) 0 active
          e       = resize $ sensor `div` nactive

controller :: Signal (Configuration, Unsigned 24) -> Signal Bool
controller = mealy controllerT (False,0)

configuredController :: Signal (Bool, Configuration, Unsigned 24) -> Signal Bool
configuredController input = controller (bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configuration (bundle (enable,config))

topEntity = configuredController