// {"signature":"swift::QueryInterfaceTypeSubstitutions::operator()(swift::SubstitutableType*) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
extension Collection where Self : a {
  struct Index protocol a
