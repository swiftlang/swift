// {"signature":"swift::ReferencedAssociatedTypesRequest::evaluate(swift::Evaluator&, swift::ValueDecl*) const::Walker::walkToTypePre(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a {
  var
  b, c : Codable {
    self
  }
}
