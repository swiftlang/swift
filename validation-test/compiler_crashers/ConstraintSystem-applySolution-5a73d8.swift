// {"kind":"typecheck","original":"43bd6d32","signature":"swift::constraints::ConstraintSystem::applySolution(swift::constraints::Solution&, swift::constraints::SyntacticElementTarget)","signatureAssert":"Assertion failed: (isValidType(type) && \"type binding has invalid type\"), function applySolution","signatureNext":"TypeChecker::typeCheckTarget"}
// RUN: not --crash %target-swift-frontend -typecheck %s
typealias a<each b, each c> = (repeat (each b each c)
0
func d<each b, each c>(repeat each b, repeat each c) -> a<repeat each b, repeat each c>
func e<each g>(f: repeat each g) -> (repeat () -> each g) {
  (repeat d {
    each f
