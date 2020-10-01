// RUN: %target-swift-frontend -typecheck %s
enum E<T> {}

protocol P {
  associatedtype T 
  var closure: () -> E<T> { get }
}

struct Concrete<Value>: P {
  let closure: () -> E<Value>
}
