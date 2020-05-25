module MaterialUI.Icons.AddCircleIcon (addCircleIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classAddCircleIcon :: forall a. ReactClass a

addCircleIcon :: ReactElement
addCircleIcon = unsafeCreateElement classAddCircleIcon {} []
