// {"signature":"swift::LazyStoragePropertyRequest::evaluate(swift::Evaluator&, swift::VarDecl*) const"}
// RUN: not %target-swift-frontend -typecheck %s
class a {
  lazy(b, c) {
