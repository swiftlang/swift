// {"signature":"swift::isOverrideBasedOnType(swift::ValueDecl const*, swift::Type, swift::ValueDecl const*)"}
// RUN: not %target-swift-frontend -typecheck %s
struct a < b {
  protocol c { associatedtype d init(e : d
  }
  class f {
    init(e : Int) {
      class 4 : f, c { init(e : d
