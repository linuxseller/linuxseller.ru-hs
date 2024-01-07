module Import where
import Prelude
import Language.Haskell.TH.Syntax
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Default (def)
import Text.Hamlet
import qualified Text.Cassius as C
import qualified Text.Julius as J
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

widgetFile = widgetFileNoReload widgetFileSettings
