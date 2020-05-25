module MaterialUI.Icons.ExpandMoreIcon (expandMoreIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classExpandMoreIcon :: forall a. ReactClass a

expandMoreIcon :: ReactElement
expandMoreIcon = unsafeCreateElement classExpandMoreIcon {} []
