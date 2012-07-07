-- import list - needs to be #included
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (bracket)

import Data.Maybe
import Data.Text (Text, unpack)
import Data.Data (Data, Typeable)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Array

import Happstack.Server

import Text.Blaze.Html5 (Html, a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Acid (AcidState, Query, Update, makeAcidic, openLocalState)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose)
import Data.SafeCopy (base, deriveSafeCopy)

