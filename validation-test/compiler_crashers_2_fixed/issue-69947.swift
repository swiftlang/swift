// RUN: %target-swift-frontend -emit-ir %s

class Thing<T> {
  var value: T
  init(_ value: T) { self.value = value }

  func combineThings<each U>(head: Thing<T>, tail: repeat Thing<each U>) {
    repeat (each tail).doSomething(each tail) { _ in }
  }

  func doSomething(_ value: AnyObject, closure: @escaping (T) -> Void) {}
}
