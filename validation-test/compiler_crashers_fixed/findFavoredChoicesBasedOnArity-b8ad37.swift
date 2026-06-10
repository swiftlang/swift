// {"kind":"typecheck","signature":"void llvm::function_ref<void (swift::constraints::Constraint*, swift::ValueDecl*, swift::FunctionType*)>::callback_fn<(anonymous namespace)::findFavoredChoicesBasedOnArity(swift::constraints::ConstraintSystem&, swift::constraints::Constraint*, swift::ArgumentList*, llvm::function_ref<void (swift::constraints::Constraint*)>)::$_0>(long, swift::constraints::Constraint*, swift::ValueDecl*, swift::FunctionType*)"}
// RUN: not %target-swift-frontend -typecheck %s
let foo: (String) -> Void = {_ in}
func foo(_ x: String) {}
foo()
