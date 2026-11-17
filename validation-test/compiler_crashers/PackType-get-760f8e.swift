// {"kind":"typecheck","original":"7d0a36fd","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (!eltTy->is<PackType>() && \"Cannot have pack directly inside another pack\"), function get","signatureNext":"TupleTypeElt"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b, each c, each d>(
  repeat each b -> each c,
  repeat each c -> each d
) -> (repeat each d) {
  a {
