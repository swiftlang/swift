// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const","signatureAssert":"Assertion failed: (found != NodeTypes.end() && \"Expected type to have been set!\"), function getType","signatureNext":"diagnoseAmbiguityWithContextualType"}
// The crash depends on the C++ parser's handling of this input.
// UNSUPPORTED: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder struct b {
  static buildBlock enum c {
    @b func d () {
      switch c {
      case a?:
      }
    }
  }
}
