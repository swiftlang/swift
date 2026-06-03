// {"kind":"complete","original":"961f1b70","signature":"swift::LifetimeDependenceInfo::getFromAST(swift::FunctionTypeRepr*, swift::AnyFunctionType*, llvm::ArrayRef<swift::LifetimeTypeAttr*>, swift::DeclContext*, swift::GenericEnvironment*)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"TypeResolver::resolveASTFunctionType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
func a<b> -> c[ {
  var : (() -> b)  #^^#
