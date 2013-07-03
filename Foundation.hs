module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (Extra (..))
import System.Log.FastLogger (Logger)

data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , appLogger :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    getLogger = return . appLogger
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)
