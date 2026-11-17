// {"kind":"complete","original":"0ccf6575","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (!eltTy->is<PackType>() && \"Cannot have pack directly inside another pack\"), function get","signatureNext":"TupleTypeElt"}
// RUN: %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b {
  associatedtype b
}
func c<each d> -> some a<(repeat each d)> {
  return e(
    #^^#
  f
  func e<each d: a>(repeat each d) -> some a<(repeat each d.b)>
