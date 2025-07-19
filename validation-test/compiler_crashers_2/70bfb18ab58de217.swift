// {"signature":"swift::GenericContext::getGenericSignature() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < let b : c
  class c < b : a
