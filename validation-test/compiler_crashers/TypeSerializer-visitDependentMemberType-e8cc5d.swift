// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"dcff4c5c","signature":"swift::serialization::Serializer::TypeSerializer::visitDependentMemberType(swift::DependentMemberType const*)","signatureAssert":"Assertion failed: (dependent->getAssocType() && \"Unchecked dependent member type\"), function visitDependentMemberType","signatureNext":"Serializer::writeASTBlockEntity"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
protocol a {
  associatedtype b
  struct c < d, e
    struct f < d : a,
    e where d.b == e
      func g<d, h> -> f<c<d.i, h>, h>
