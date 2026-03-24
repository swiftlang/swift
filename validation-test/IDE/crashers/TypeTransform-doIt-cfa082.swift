// {"kind":"complete","original":"01b22364","signature":"swift::TypeTransform<swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const::Transform>::doIt(swift::Type, swift::TypePosition)","signatureAssert":"Assertion failed: (!substType->hasTypeParameter()), function operator()","signatureNext":"Type::transformRec"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a {
  associatedtype b
  associatedtype c where b == c
}
func d<e {
  struct f: a {
    typealias b = e
    typealias #^^#
