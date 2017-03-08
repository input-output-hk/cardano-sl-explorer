module Explorer.Api.Http where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens ((^.))
import Debug.Trace (trace, traceAny)
import Explorer.Api.Helper (decodeResult)
import Explorer.Api.Types (EndpointError(..), Endpoint)
import Explorer.Types.State (CBlockEntries, CTxEntries)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash)
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CAddress)

endpointPrefix :: String
-- endpointPrefix = "http://localhost:8100/api/"
endpointPrefix = "/api/"

-- result helper

decodeResponse :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResponse = decodeResult <<< _.response

request :: forall a r eff. (Generic a, Requestable r) => AffjaxRequest r ->
    Endpoint -> Aff (ajax :: AJAX | eff) a
request req endpoint = do
    result <- affjax $ req { url = endpointPrefix <> endpoint }
    when (isHttpError result.status) $
        throwError <<< error <<< show $ HTTPStatusError result
    either throwError pure $ decodeResponse result
    where
      isHttpError (StatusCode c) = c >= 400

get :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
get e = trace "get" \_ -> traceAny e \_ -> request defaultRequest e

post :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
post = request $ defaultRequest { method = Left POST }

-- api

-- blocks
fetchLatestBlocks :: forall eff. Aff (ajax::AJAX | eff) CBlockEntries
fetchLatestBlocks = get "blocks/last"

fetchBlockSummary :: forall eff. CHash -> Aff (ajax::AJAX | eff) CBlockSummary
fetchBlockSummary hash = get $ "blocks/summary/" <> hash ^. _CHash

fetchBlockTxs :: forall eff. CHash -> Aff (ajax::AJAX | eff) CTxEntries
fetchBlockTxs hash = get $ "blocks/txs/" <> hash ^. _CHash

-- txs
fetchLatestTxs :: forall eff. Aff (ajax::AJAX | eff) CTxEntries
fetchLatestTxs = get "txs/last"

-- addresses
fetchAddressSummary :: forall eff. CAddress -> Aff (ajax::AJAX | eff) CAddressSummary
fetchAddressSummary address = get $ "addresses/summary/" <> address ^. _CAddress