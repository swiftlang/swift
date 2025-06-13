// {"signature":"swift::EnumRawValuesRequest::evaluate(swift::Evaluator&, swift::EnumDecl*, swift::TypeResolutionStage) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
func a<each b>(c : repeat each b) {                 repeat !(d c
