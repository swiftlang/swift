// {"kind":"emit-sil","original":"af5c6219","signature":"(anonymous namespace)::LifetimeChecker::doIt()"}
// RUN: not --crash %target-swift-frontend -emit-sil %s
struct a<b> {
  let c: String
  let d: b
}
extension a where b == Void {
  init() {
    d <= ()
  }
}
