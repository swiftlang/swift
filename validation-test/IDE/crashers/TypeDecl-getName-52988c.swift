// {"kind":"complete","original":"44e94bb2","signature":"swift::TypeDecl::getName() const","signatureAssert":"Assertion failed: (!canType->hasUnboundGenericType()), function computeInvertibleConformances"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
extension {
  extension Optional ...{== { #^^#
