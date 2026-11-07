// {"kind":"typecheck","signature":"swift::AnyFunctionType* swift::TypeBase::castTo<swift::AnyFunctionType>()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"MissingOptionalUnwrapFailure::diagnoseAsError"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript(a: Int?) -> <#type#> {
  var b = a
  Float(b)
}
