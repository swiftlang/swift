// {"signature":"swift::Decl::getLoc(bool) const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@_typeEraser(a) protocol b dynamic func c->some b {
  d
