// RUN: %target-typecheck-verify-swift

extension Array where Element == String {
  typealias InternalTypeAlias = Int
  func doSomething<R>(foo: (InternalTypeAlias) -> R) {}
}
