// RUN: %target-run-simple-swift
// REQUIRES: executable_test

class MyLabel {
  var text = "label"
}

class Controller {
  fileprivate let label = MyLabel()
}

struct Container<V> {
  var v : V
  init(_ v: V) {
    self.v = v
  }
  func useKeyPath<V2: AnyObject>(_ keyPath: KeyPath<V, V2>) -> String {
    return (v[keyPath: keyPath] as! MyLabel).text
  }
}

extension Container where V: Controller {
  func test() -> String {
    return useKeyPath(\.label)
  }
}
// CHECK: label
print(Container(Controller()).test())
