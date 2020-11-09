// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -o /dev/null %s
// REQUIRES: asserts

final class Klass {
  static var current: Klass {
    fatalError()
  }
}
private struct Build<T> {
  let val: T
  unowned let unownedBinding = Klass.current
  unowned(unsafe) let unownedUnsafeBinding = Klass.current
  weak var weakBinding = Klass.current
}
private func phase<T>(_ val: T) -> Build<T> {
  return Build<T>(val: val)
}
