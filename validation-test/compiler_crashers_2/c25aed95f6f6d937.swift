// {"signature":"swift::TypeDecl::getName() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  extension {
    a {
      func b {
        super
