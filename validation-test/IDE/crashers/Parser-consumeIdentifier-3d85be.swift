// {"kind":"complete","signature":"swift::Parser::consumeIdentifier(swift::Identifier&, bool)","signatureAssert":"Assertion failed: (Tok.isAny(tok::identifier, tok::kw_self, tok::kw_Self)), function consumeIdentifier"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
switch { case borrowing #^COMPLETE^#
