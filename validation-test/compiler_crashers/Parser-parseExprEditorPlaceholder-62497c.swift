// {"kind":"typecheck","original":"21bee1da","signature":"swift::Parser::parseExprEditorPlaceholder(swift::Token, swift::Identifier)","signatureAssert":"Assertion failed: (PlaceholderId.isEditorPlaceholder()), function parseExprEditorPlaceholder","signatureNext":"Parser::parseExprIdentifier"}
// RUN: not --crash %target-swift-frontend -typecheck %s
<##>::
