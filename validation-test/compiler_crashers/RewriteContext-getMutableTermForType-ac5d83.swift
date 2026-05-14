// {"extraArgs":["-experimental-allow-module-with-compiler-errors"],"kind":"emit-sil","original":"03ece0b0","signature":"swift::rewriting::RewriteContext::getMutableTermForType(swift::CanType, swift::ProtocolDecl const*)","signatureAssert":"Assertion failed: (proto->getSelfInterfaceType()->isEqual(paramType)), function getMutableTermForType","signatureNext":"RuleBuilder::addRequirement"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors %s
protocol a {
  struct b<c, d> {
    extension <#type#> {
      protocol e where f == d {
      }
    }
  }
}
