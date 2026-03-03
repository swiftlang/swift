// {"kind":"typecheck","signature":"swift::constraints::SpecifyLabelToAssociateTrailingClosure::diagnose(swift::constraints::Solution const&, bool) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a {}
func a<each b, c>(repeat each b, c)
