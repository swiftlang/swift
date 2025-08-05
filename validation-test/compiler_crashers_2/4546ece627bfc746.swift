// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::addFix(swift::constraints::ConstraintFix*)","signatureAssert":"Assertion failed: (inserted), function addFix"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<b>(b) -> b a(["":0]) as [Int:String]
