module React.DOM.NonBlockingSpace where

import React (ReactElement)
import React.DOM (span)
import React.DOM.Props (dangerouslySetInnerHTML) as RP

nbsp :: ReactElement
nbsp = span [ RP.dangerouslySetInnerHTML { __html: "&nbsp;" } ] []
