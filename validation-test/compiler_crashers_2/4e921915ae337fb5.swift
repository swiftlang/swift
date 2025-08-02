// {"kind":"typecheck","signature":"swift::ConditionalRequirementsRequest::evaluate(swift::Evaluator&, swift::NormalProtocolConformance*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a typealias b<c> = () extension b : a
