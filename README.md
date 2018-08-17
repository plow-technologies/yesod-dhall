# Helpers to handle Dhall in Yesod routes

This package provides a response type for sending Dhall-encoded values
as Yesod HTTP responses, and for parsing POST bodies as Dhall in Yesod handlers.

Import: `Yesod.Dhall`

Parse a POST body as Dhall: `requireDhallBody`.

Send a response encoded as Dhall (or a plain text error): `sendDhallResponseWithStatus`
