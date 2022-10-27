{-# LANGUAGE UndecidableInstances #-}
module Servant.API.TypeLevel.Prune
  ( Prune
  , Wild
  )
  where

import           Servant.API
import           Servant.API.TypeLevel


-- | On the left hand side of 'IsElem' and 'Prune',
-- @UVerb ... ... '[Wild]@ will match any return result type.
data Wild
type instance IsElem' (UVerb m ct '[Wild]) (Verb m s ct' _) = IsSubList ct ct'
type instance IsElem' (UVerb m ct '[Wild]) (UVerb m ct' _) = IsSubList ct ct'

type instance IsElem' sa (Description _ :> sb) = IsElem sa sb
type instance IsElem' sa (AuthProtect _ :> sb) = IsElem sa sb
type instance IsElem' sa (QueryParam' _ _ _ :> sb) = IsElem sa sb

-- | Restrict the API in @api@ to the endpoints in @mask@. This works similarly
-- to 'IsElem' in that the @mask@ doesn't have to specify non-path parameters.
-- Additionally, 'Wild' can be used as a wildcard for the return type of a
-- request.
type family Prune (mask :: *) (api :: *) :: * where
  -- The instances mimic 'IsElem' and the 'IsElem'' instances above
  Prune sa (sb :<|> sc) = Prune sa sb ?<|> Prune sa sc
  Prune (e :> sa) (e :> sb) = e ?> Prune sa sb
  Prune sa (Header sym x :> sb) = Header sym x ?> Prune sa sb
  Prune sa (ReqBody y x :> sb) = ReqBody y x ?> Prune sa sb
  Prune (CaptureAll z y :> sa) (CaptureAll x y :> sb) = CaptureAll x y ?> Prune sa sb
  Prune (Capture z y :> sa) (Capture x y :> sb) = Capture x y ?> Prune sa sb
  Prune sa (QueryParam' x y z :> sb) = QueryParam' x y z ?> Prune sa sb
  Prune sa (QueryParams x y :> sb) = QueryParams x y ?> Prune sa sb
  Prune sa (QueryFlag x :> sb) = QueryFlag x ?> Prune sa sb
  Prune sa (Fragment x :> sb) = Fragment x ?> Prune sa sb
  Prune sa (Description x :> sb) = Description x ?> Prune sa sb
  Prune sa (AuthProtect x :> sb) = AuthProtect x ?> Prune sa sb
  Prune (Verb m s ct typ) (Verb m s ct typ) = Verb m s ct typ
  Prune (UVerb m ct typs) (UVerb m ct typs) = UVerb m ct typs
  Prune (UVerb m ct '[Wild]) (Verb m s ct typ) = Verb m s ct typ
  Prune (UVerb m ct '[Wild]) (UVerb m ct typs) = UVerb m ct typs
  Prune (sa :<|> sb) sc = Prune sa sc ?<|> Prune sb sc
  Prune sa sb = EmptyAPI

type family (?>) (path :: k) (api :: *) :: * where
  path ?> EmptyAPI = EmptyAPI
  path ?> api = path :> api

type family (?<|>) (api :: *) (api' :: *) :: * where
  EmptyAPI ?<|> sa = sa
  sa ?<|> EmptyAPI = sa
  sa ?<|> sb = sa :<|> sb
