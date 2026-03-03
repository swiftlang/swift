// RUN: not %target-swift-frontend -typecheck %s
extension Result {
  func a<each b>() where Success == (Result) -> (repeat each b)> {}
}
