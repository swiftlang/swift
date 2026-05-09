// {"kind":"complete","original":"284a44b6","signature":"swift::rewriting::RewriteContext::getMutableTermForType(swift::CanType, swift::ProtocolDecl const*)","signatureAssert":"Assertion failed: (paramType->isTypeParameter()), function getMutableTermForType","signatureNext":"RequirementMachine::isValidTypeParameter"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b> {
  associatedtype b
}
struct c<b, d, e: a<b>, f> {
  struct g<b, h: c<b, a, a, Error>> {
    i { #^^#
      }
  }
