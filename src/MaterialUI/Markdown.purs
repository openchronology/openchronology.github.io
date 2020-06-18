module MaterialUI.Markdown where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import React (ReactElement, statelessComponent, childrenToArray)
import React.DOM (code, ul, ol, blockquote) as R
import React.DOM.Props (style) as RP
import React.DOM.Markdown (markdown) as RM
import React.DOM.Highlight (highlight)
import MaterialUI.Typography (typography)
import MaterialUI.Divider (divider')
import MaterialUI.Enums (h5, h6, subtitle1, h4, h3, h2, body1, caption)
import Partial.Unsafe (unsafePartial)

markdown :: String -> ReactElement
markdown source =
  RM.markdown
    { source
    , renderers:
        { heading:
            statelessComponent \{ level, children } ->
              unsafePartial
                $ case level of
                    1 -> typography { variant: h2 } (childrenToArray children)
                    2 -> typography { variant: h3 } (childrenToArray children)
                    3 -> typography { variant: h4 } (childrenToArray children)
                    4 -> typography { variant: h5 } (childrenToArray children)
                    5 -> typography { variant: h6 } (childrenToArray children)
                    6 -> typography { variant: subtitle1 } (childrenToArray children)
        , paragraph: statelessComponent \{ children } -> typography { variant: body1, paragraph: true } (childrenToArray children)
        , thematicBreak: statelessComponent \_ -> divider' { style: { margin: "1em 0" } }
        , blockquote:
            statelessComponent \{ children } ->
              typography { variant: caption }
                [ R.blockquote [] (childrenToArray children) ]
        , list:
            statelessComponent \{ children, ordered } ->
              typography { variant: body1, component: "div" }
                [ if ordered then
                    R.ol [] (childrenToArray children)
                  else
                    R.ul [] (childrenToArray children)
                ]
        , inlineCode: statelessComponent \{ children } -> R.code [ RP.style { border: "1px solid #ddd" } ] (childrenToArray children)
        , code:
            statelessComponent \{ language, value } ->
              let
                codeProps = case toMaybe language of
                  Nothing -> { language: "" }
                  Just l
                    | l == "purescript" -> { language: "haskell" }
                    | otherwise -> { language: l }
              in
                highlight codeProps (childrenToArray value)
        }
    }
