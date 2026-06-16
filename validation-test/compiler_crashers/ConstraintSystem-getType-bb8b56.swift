// {"extraArgs":["-language-mode","6","-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"c82bd1f3","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"ConstraintSystem::simplifySyntacticElementConstraint"}
// RUN: not --crash %target-swift-frontend -emit-sil -language-mode 6 -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
{
  struct a {
    var b =
      if .random() {
        throw <#expression#>
      }
  }
}
