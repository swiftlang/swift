// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"2a782ae8","signature":"swift::serialization::Serializer::DeclSerializer::visitEnumElementDecl(swift::EnumElementDecl const*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
// REQUIRES: objc_interop
@objc enum a: Int {
  case  = 0.1.1
}
