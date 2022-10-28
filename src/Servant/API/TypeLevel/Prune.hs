{-# LANGUAGE UndecidableInstances #-}
module Servant.API.TypeLevel.Prune
  ( Prune
  , Wild
  )
  where

import           Data.Kind (Type)
import           Servant.API
import           Servant.API.TypeLevel


-- | On the left hand side of 'IsElem' and 'Prune',
-- @UVerb ... ... '[Wild]@ will match any return result type.
data Wild
type instance IsElem' (Verb m s ct Wild) (Verb m s ct' _) = IsSubList ct ct'
type instance IsElem' (UVerb m ct '[Wild]) (Verb m s ct' _) = IsSubList ct ct'
type instance IsElem' (UVerb m ct '[Wild]) (UVerb m ct' _) = IsSubList ct ct'

-- things that have 'HasLink' instances but not 'IsElem' instances
type instance IsElem' (Capture' _ y :> sa) (Capture' x y :> sb) = IsElem sa sb
type instance IsElem' sa (HttpVersion         :> sb) = IsElem sa sb
type instance IsElem' sa (Vault               :> sb) = IsElem sa sb
type instance IsElem' sa (BasicAuth   _ _     :> sb) = IsElem sa sb
type instance IsElem' sa (Description _       :> sb) = IsElem sa sb
type instance IsElem' sa (Summary     _       :> sb) = IsElem sa sb
type instance IsElem' sa (AuthProtect _       :> sb) = IsElem sa sb
type instance IsElem' sa (IsSecure            :> sb) = IsElem sa sb
type instance IsElem' sa (Header'     _ _ _   :> sb) = IsElem sa sb
type instance IsElem' sa (QueryParam' _ _ _   :> sb) = IsElem sa sb
type instance IsElem' sa (RemoteHost          :> sb) = IsElem sa sb
type instance IsElem' sa (ReqBody'    _ _ _   :> sb) = IsElem sa sb
type instance IsElem' sa (StreamBody' _ _ _ _ :> sb) = IsElem sa sb

-- | Restrict the API in @api@ to the endpoints in @mask@. This works similarly
-- to 'IsElem' in that the @mask@ doesn't have to specify non-path parameters.
-- Additionally, 'Wild' can be used as a wildcard for the return type of a
-- request.
type family Prune (mask :: Type) (api :: Type) :: Type where
  -- The instances mimic 'IsElem' and the 'IsElem'' instances above
  Prune sa (sb :<|> sc) = Prune sa sb ?<|> Prune sa sc
  Prune (e :> sa) (e :> sb) = e ?> Prune sa sb
  Prune (CaptureAll _ y :> sa) (CaptureAll x y :> sb) = CaptureAll x y ?> Prune sa sb
  Prune (Capture' _ _ z :> sa) (Capture' x y z :> sb) = Capture' x y z ?> Prune sa sb
  Prune sa (HttpVersion         :> sb) = HttpVersion         ?> Prune sa sb
  Prune sa (Vault               :> sb) = Vault               ?> Prune sa sb
  Prune sa (BasicAuth   x y     :> sb) = BasicAuth   x y     ?> Prune sa sb
  Prune sa (Description x       :> sb) = Description x       ?> Prune sa sb
  Prune sa (Summary     x       :> sb) = Summary     x       ?> Prune sa sb
  Prune sa (AuthProtect x       :> sb) = AuthProtect x       ?> Prune sa sb
  Prune sa (Fragment    x       :> sb) = Fragment    x       ?> Prune sa sb
  Prune sa (IsSecure            :> sb) = IsSecure            ?> Prune sa sb
  Prune sa (Header'     x y z   :> sb) = Header'     x y z   ?> Prune sa sb
  Prune sa (QueryFlag   x       :> sb) = QueryFlag   x       ?> Prune sa sb
  Prune sa (QueryParams x y     :> sb) = QueryParams x y     ?> Prune sa sb
  Prune sa (QueryParam' x y z   :> sb) = QueryParam' x y z   ?> Prune sa sb
  Prune sa (RemoteHost          :> sb) = RemoteHost          ?> Prune sa sb
  Prune sa (ReqBody'    x y z   :> sb) = ReqBody'    x y z   ?> Prune sa sb
  Prune sa (StreamBody' x y z w :> sb) = StreamBody' x y z w ?> Prune sa sb
  Prune (Verb m s ct typ) (Verb m s ct typ) = Verb m s ct typ
  Prune (Verb m s ct Wild) (Verb m s ct typ) = Verb m s ct typ
  Prune (UVerb m ct typs) (UVerb m ct typs) = UVerb m ct typs
  Prune (UVerb m ct '[Wild]) (Verb m s ct typ) = Verb m s ct typ
  Prune (UVerb m ct '[Wild]) (UVerb m ct typs) = UVerb m ct typs
  Prune (sa :<|> sb) sc = Prune sa sc ?<|> Prune sb sc
  Prune sa sb = EmptyAPI

type family (?>) (path :: k) (api :: Type) :: Type where
  path ?> EmptyAPI = EmptyAPI
  path ?> api = path :> api

type family (?<|>) (api :: Type) (api' :: Type) :: Type where
  EmptyAPI ?<|> sa = sa
  sa ?<|> EmptyAPI = sa
  sa ?<|> sb = sa :<|> sb
