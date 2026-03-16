// {"kind":"typecheck","original":"d2ffa4d1","signature":"swift::constraints::ConstraintSystem::simplifySyntacticElementConstraint(swift::ASTNode, swift::constraints::ContextualTypeInfo, bool, swift::optionset::OptionSet<swift::constraints::ConstraintSystem::TypeMatchFlags, unsigned int>, swift::constraints::ConstraintLocatorBuilder)","signatureNext":"ConstraintSystem::simplifyConstraint"}
// RUN: not --crash %target-swift-frontend -typecheck %s
subscript() -> <#type#> {
  _read {
    !if a.b {
      yield
    }
  }
}
