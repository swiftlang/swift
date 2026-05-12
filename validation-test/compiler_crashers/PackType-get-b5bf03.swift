// {"kind":"typecheck","original":"90279022","signature":"swift::PackType::get(swift::ASTContext const&, llvm::ArrayRef<swift::Type>)","signatureAssert":"Assertion failed: (!eltTy->is<PackType>() && \"Cannot have pack directly inside another pack\"), function get","signatureNext":"TypeBase::getContextSubstitutions"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b> -> b {
  c(f
}
func c<each d, each e>(repeat each d, repeat (each d -> each e)) -> (repeat each e
