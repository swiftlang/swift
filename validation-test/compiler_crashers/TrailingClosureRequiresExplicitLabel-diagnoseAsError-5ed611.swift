// {"kind":"typecheck","signature":"swift::constraints::TrailingClosureRequiresExplicitLabel::diagnoseAsError()","signatureNext":"SpecifyLabelToAssociateTrailingClosure::diagnose"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a {}
func a<each b, c>(repeat each b, c)
