// {"kind":"complete","original":"aff2bc20","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (!eltTy->is<PackType>() && \"Cannot have pack directly inside another pack\"), function get","signatureNext":"TupleTypeElt"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
protocol a<b, c {
  associatedtype b
  associatedtype c
}
struct d<each e: a>: a {
  typealias b = (repeat each e.b)
  init(repeat each e)
}
func f<c, each e> -> some a<(repeat each e), c> {
  d(#^^#
  g
