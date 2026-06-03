// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"ea518c78","signature":"swift::serialization::Serializer::DeclSerializer::writeDeclAttribute(swift::Decl const*, swift::DeclAttribute const*)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
a {
  @_typeEraser(b) struct c {
  }
}
