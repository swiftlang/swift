// {"signature":"swift::constraints::Solution::getFunctionArgApplyInfo(swift::constraints::ConstraintLocator*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: asserts
func a<b, each c>(d : repeat(b)->each c) {                               repeat d(e = f
