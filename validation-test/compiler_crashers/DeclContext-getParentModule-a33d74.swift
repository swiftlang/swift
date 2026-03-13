// {"kind":"typecheck","original":"16240db9","signature":"swift::DeclContext::getParentModule() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@a({
  unowned protocol b
  }
  let c
