// {"kind":"typecheck","signature":"(anonymous namespace)::ExprWalker::rewriteTarget(swift::constraints::SyntacticElementTarget)"}
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
