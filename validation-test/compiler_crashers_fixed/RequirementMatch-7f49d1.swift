// {"kind":"typecheck","original":"19b50bfb","signature":"swift::RequirementMatch llvm::function_ref<swift::RequirementMatch (bool, llvm::ArrayRef<swift::OptionalAdjustment>)>::callback_fn<swift::matchWitness(llvm::DenseMap<std::__1::pair<swift::GenericSignatureImpl const*, swift::ClassDecl const*>, swift::RequirementEnvironment, llvm::DenseMapInfo<std::__1::pair<swift::GenericSignatureImpl const*, swift::ClassDecl const*>, void>, llvm::detail::DenseMapPair<std::__1::pair<swift::GenericSignatureImpl const*, swift::ClassDecl const*>, swift::RequirementEnvironment>>&, swift::ProtocolDecl*, swift::RootProtocolConformance*, swift::DeclContext*, swift::ValueDecl*, swift::ValueDecl*)::$_2>(long, bool, llvm::ArrayRef<swift::OptionalAdjustment>)","signatureAssert":"Assertion failed: (getEffects(req).contains(getEffects(witness)) && \"witness has more effects than requirement?\"), function operator()"}
// RUN: not %target-swift-frontend -typecheck %s
protocol a {
  associatedtype Element
  ::
  func b(c: Element
}
extension Array: a {
    b( Element) throws(Never
