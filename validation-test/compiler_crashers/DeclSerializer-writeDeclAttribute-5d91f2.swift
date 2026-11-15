// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"1c2a69aa","signature":"swift::serialization::Serializer::DeclSerializer::writeDeclAttribute(swift::Decl const*, swift::DeclAttribute const*)","signatureAssert":"Assertion failed: (afd && \"Missing replaced decl!\"), function writeDeclAttribute","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
a {
  struct b {
    @_dynamicReplacement(for:) var c: <#type#>
  }
}
