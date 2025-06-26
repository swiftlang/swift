// {"signature":"swift::PatternTypeRequest::evaluate(swift::Evaluator&, swift::ContextualPattern) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
@objcMembers class a open extension a {
  var b
