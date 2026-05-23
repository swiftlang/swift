// {"kind":"complete","original":"eb9db465","signature":"swift::Mangle::ASTMangler::appendType(swift::Type, swift::GenericSignature, swift::ValueDecl const*)","signatureNext":"Mangle::ASTMangler::appendBoundGenericArgs"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  [$0..<Swift
  #^^#
