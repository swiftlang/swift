// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -o /dev/null %s

// https://github.com/apple/swift/issues/55903

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
