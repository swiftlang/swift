// {"kind":"complete","original":"df6ac250","signature":"bool llvm::function_ref<bool (swift::constraints::Constraint*)>::callback_fn<swift::constraints::ConstraintSystem::simplifyAppliedOverloadsImpl(swift::constraints::Constraint*, swift::TypeVariableType*, swift::FunctionType*, unsigned int, swift::constraints::ConstraintLocatorBuilder)::$_2>(long, swift::constraints::Constraint*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
struct a {
b func b()
  #if {
    b(<#expression#>)
  }#^^#
  #endif
}
