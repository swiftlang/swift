// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"4b15da54","signature":"swift::TypeBase::replaceSelfParameterType(swift::Type)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"TypeBase::adjustSuperclassMemberDeclType"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
class a {
  var b: String {
    get
  }
  class c: a {
    var b: String {
      {
      }
    }
  }
}
