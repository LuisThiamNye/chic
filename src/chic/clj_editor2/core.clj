(ns chic.clj-editor2.core
  (:require
   [chic.clj-editor2.ui-base :as ui-base]))

(defn sample-view []
  (ui-base/sample-view))

"
visual elements:
- text
- cursor
- select highlight
layout:
- lines
   - line spacing
- sub-lines (wrapping)
- AST nodes (including whitespace)
- segments (nodes split across lines; inserted elements)
- embedded view
overlays:
- docs
- eval results

editor state:
- active line (for scroll position)
- active line scroll displacement
- ast

cursor position
-
"
