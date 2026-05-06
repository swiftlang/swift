// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"e53a752e","signature":"swift::serialization::Serializer::DeclSerializer::writeDeclAttribute(swift::Decl const*, swift::DeclAttribute const*)","signatureAssert":"Assertion failed: (paramIndices && \"Parameter indices must be resolved\"), function writeDeclAttribute","signatureNext":"Serializer::DeclSerializer::visit"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
a {
  struct b {
    @differentiable() var c: <#type#>
  }
}
