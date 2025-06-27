// {"signature":"swift::constraints::TrailingClosureRequiresExplicitLabel::fixIt(swift::InFlightDiagnostic&, swift::constraints::FunctionArgApplyInfo const&) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
a {}
func a<each b, c>(repeat each b, c)
