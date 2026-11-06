// {"kind":"typecheck","original":"21bee1da","signature":"swift::Parser::parseExprEditorPlaceholder(swift::Token, swift::Identifier)","signatureAssert":"Assertion failed: (PlaceholderId.isEditorPlaceholder()), function parseExprEditorPlaceholder","signatureNext":"Parser::parseExprIdentifier"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
<##>::
