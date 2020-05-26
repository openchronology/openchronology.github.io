module MaterialUI.Markdown where

import React (ReactElement, createLeafElement)
import React.DOM.Markdown (markdown) as RM


markdown :: String -> ReactElement
markdown source = RM.markdown
  { source
  , renderers: {}
  }
