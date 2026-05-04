// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"386699b3","signature":"swift::serialization::Serializer::DeclSerializer::visitEnumElementDecl(swift::EnumElementDecl const*)","signatureAssert":"Assertion failed: (Val && \"isa<> used on a null pointer\"), function doit","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
// REQUIRES: objc_interop
@objc enum a: Int {
  case ()
}
