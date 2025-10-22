// {"kind":"typecheck","original":"54198387","signature":"swift::Expr* llvm::function_ref<swift::Expr* (swift::Expr*)>::callback_fn<fixMissingArguments(swift::constraints::ConstraintSystem&, swift::ASTNode, llvm::SmallVectorImpl<swift::AnyFunctionType::Param>&, llvm::ArrayRef<swift::AnyFunctionType::Param>, unsigned int, swift::constraints::ConstraintLocatorBuilder)::$_0>(long, swift::Expr*)","signatureAssert":"Assertion failed: (!baseName.isSpecial() && \"Can't retrieve the identifier of a special base name\"), function getBaseIdentifier"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(
  c: (b, ()) -> b = {
    $0.init
  })
