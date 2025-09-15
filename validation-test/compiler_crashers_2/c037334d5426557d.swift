// {"kind":"typecheck","signature":"swift::constraints::ConstraintSystem::applySolutionFixes(swift::constraints::Solution const&)","aliases":["swift::constraints::AllowArgumentMismatch::create(swift::constraints::ConstraintSystem&, swift::Type, swift::Type, swift::constraints::ConstraintLocator*)"]}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(repeat each b, repeat each b)
a(repeat (
