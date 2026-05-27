// {"kind":"complete","original":"52902d58","signature":"parseQualifiedDeclName(swift::Parser&, swift::Diag<>, swift::TypeRepr*&, swift::DeclNameRefWithLoc&)","signatureAssert":"Assertion failed: (P.startsWithSymbol(P.Tok, '.') && \"false\"), function parseBaseTypeForQualifiedDeclName","signatureNext":"Parser::parseDerivativeAttribute"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@derivative(of: a.#^^#)
<#declaration#>
