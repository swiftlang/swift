// {"kind":"typecheck","signature":"(anonymous namespace)::ExprWalker::rewriteTarget(swift::constraints::SyntacticElementTarget)"}
// RUN: not %target-swift-frontend -typecheck %s
a < {
  AnyObject ( <b)
}
