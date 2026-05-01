// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"2b4d2148","signature":"swift::serialization::Serializer::DeclSerializer::writeDeclAttribute(swift::Decl const*, swift::DeclAttribute const*)","signatureAssert":"Assertion failed: (afd && \"Missing replaced decl!\"), function writeDeclAttribute","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
extension <#type#> {
  @_dynamicReplacement(for:) var a: <#type#>
}
