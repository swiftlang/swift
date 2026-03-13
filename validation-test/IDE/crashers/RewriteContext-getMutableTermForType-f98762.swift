// {"kind":"complete","signature":"swift::rewriting::RewriteContext::getMutableTermForType(swift::CanType, swift::ProtocolDecl const*)","signatureAssert":"Assertion failed: (paramType->isTypeParameter()), function getMutableTermForType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a { associatedtype b }
protocol c { associatedtype b }
extension c { compose<d where d ==a, d.b == b { #^COMPLETE^#
