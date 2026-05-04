// {"kind":"typecheck","original":"a54dbd9e","signature":"swift::constraints::ConstraintSystem::getCalleeLocator(swift::constraints::ConstraintLocator*, bool, llvm::function_ref<swift::Type (swift::Expr*)>, llvm::function_ref<swift::Type (swift::Type)>, llvm::function_ref<std::__1::optional<swift::constraints::SelectedOverload> (swift::constraints::ConstraintLocator*)>)","signatureAssert":"Assertion failed: (!fnTy->is<TypeVariableType>()), function operator()","signatureNext":"ConstraintSystem::matchFunctionResultTypes"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a(: ()) -> () -> Void
a()(<#expression#>)
func a(: ()) -> () -> Void
