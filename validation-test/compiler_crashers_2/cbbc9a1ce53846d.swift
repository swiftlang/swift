// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::getType(swift::ASTNode) const"}
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
