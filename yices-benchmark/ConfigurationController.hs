module ConfigurationController where

import CLaSH.Prelude

configurationControllerT :: c -> (Bool, c) -> (c,c)
configurationControllerT oldConfig (enable,config) = 
  if enable then (config,config) else (oldConfig,oldConfig)

configurationController :: c -> Signal (Bool, c) -> Signal c
configurationController = mealy configurationControllerT
