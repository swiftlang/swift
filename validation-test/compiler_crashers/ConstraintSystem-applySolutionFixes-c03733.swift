// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::applySolutionFixes(swift::constraints::Solution const&)","signatureNext":"ConstraintSystem::applySolution"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(repeat each b, repeat each b)
a(repeat (
