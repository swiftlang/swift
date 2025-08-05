// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getCalleeLocator(swift::constraints::ConstraintLocator*, bool, llvm::function_ref<swift::Type (swift::Expr*)>, llvm::function_ref<swift::Type (swift::Type)>, llvm::function_ref<std::__1::optional<swift::constraints::SelectedOverload> (swift::constraints::ConstraintLocator*)>)","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a func b(c : [Int]) {
  \ a(c.map{})
