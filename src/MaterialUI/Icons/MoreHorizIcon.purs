module MaterialUI.Icons.MoreHorizIcon (moreHorizIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classMoreHorizIcon :: forall a. ReactClass a


moreHorizIcon :: ReactElement
moreHorizIcon = unsafeCreateElement classMoreHorizIcon {} []
