// {"kind":"typecheck","original":"901f9c6d","signature":"swift::constraints::ConstraintSystem::getCalleeLocator(swift::constraints::ConstraintLocator*, bool, llvm::function_ref<swift::Type (swift::Expr*)>, llvm::function_ref<swift::Type (swift::Type)>, llvm::function_ref<std::__1::optional<swift::constraints::SelectedOverload> (swift::constraints::ConstraintLocator*)>)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
let a = [].reduce([]) {
  0 && a($1 == a
  $1 {
