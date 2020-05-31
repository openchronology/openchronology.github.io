module React.DOM.Highlight (highlight) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import highlightImpl :: forall a. ReactClass a

highlight :: { language :: String } -> Array ReactElement -> ReactElement
highlight = unsafeCreateElement highlightImpl
