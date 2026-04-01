// {"kind":"complete","original":"ba76788e","signature":"getTypeForSymbolRange(swift::rewriting::Symbol const*, swift::rewriting::Symbol const*, llvm::ArrayRef<swift::GenericTypeParamType*>, swift::rewriting::PropertyMap const&)","signatureAssert":"Assertion failed: (std::find(conformsTo.begin(), conformsTo.end(), symbol.getProtocol()) != conformsTo.end()), function getTypeForSymbolRange","signatureNext":"TypeTransform::doIt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a<b, c: Collection {
  d ->
    a<
      #^^#
    >
  where b == [c], c == [c.e]
