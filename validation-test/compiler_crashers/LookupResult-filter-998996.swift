// {"kind":"typecheck","original":"6484d8d4","signature":"swift::LookupResult::filter(llvm::function_ref<bool (swift::LookupResultEntry, bool)>)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"hasValidDynamicCallableMethod"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@dynamicCallable enum a {
  case dynamicallyCall(withKeywordArguments: <#type#>)
}
