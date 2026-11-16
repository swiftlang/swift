// {"frontendArgs":["-emit-sil","-debugger-testing-transform"],"kind":"custom","signature":"(anonymous namespace)::ConstraintWalker::walkToExprPost(swift::Expr*)","signatureNext":"Traversal::doIt"}
// RUN: not --crash %target-swift-frontend -emit-sil -debugger-testing-transform %s
func d(a: Int, b: Int) {
  var c = 1
  c = a ?? b
}
