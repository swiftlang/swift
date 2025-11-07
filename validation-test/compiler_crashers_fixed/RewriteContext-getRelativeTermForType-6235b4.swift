// RUN: not %target-swift-frontend -typecheck %s
// https://github.com/swiftlang/swift/issues/84490
struct a < b > {
  func
    c < each d where (repeat each d , b) == b>()
}
