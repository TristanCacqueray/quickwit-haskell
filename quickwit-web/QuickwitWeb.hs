module Main where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Context (EmptyContext), Get, NamedRoutes, OctetStream, (:-), (:>))
import Servant qualified
import Servant.HTML.Lucid (HTML)

import RIO

-- lucid to write html
import Lucid qualified

data API mode = API
    { index :: mode :- Get '[HTML] (Lucid.Html ())
    , fav :: mode :- "favicon.ico" :> Get '[OctetStream] ByteString
    }
    deriving (Generic)

app :: Wai.Application
app =
    Servant.serveWithContext
        (Proxy @(NamedRoutes API))
        EmptyContext
        API
            { index = pure $ Lucid.doctypehtml_ do
                Lucid.head_ do
                    Lucid.title_ "Quickwit Web demo"
                Lucid.body_ do
                    "Welcome!"
            , fav = pure ""
            }

main :: IO ()
main = do
    putStrLn "Serving to http://localhost:8000"
    Warp.run 8000 app
