// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::applySolutionFixes(swift::constraints::Solution const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c: repeat (each b), d: repeat each b)
a(d)
