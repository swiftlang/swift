// {"kind":"typecheck","signature":"void llvm::function_ref<void (swift::constraints::Constraint*, swift::ValueDecl*, swift::FunctionType*)>::callback_fn<(anonymous namespace)::findFavoredChoicesBasedOnArity(swift::constraints::ConstraintSystem&, swift::constraints::Constraint*, swift::ArgumentList*, llvm::function_ref<void (swift::constraints::Constraint*)>)::$_0>(long, swift::constraints::Constraint*, swift::ValueDecl*, swift::FunctionType*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
var a: Any
let a : () -> Void = a (b)
