// RUN: %target-run-simple-swift | %FileCheck %s
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

public class GenericController<U> {
  init(_ u: U) {
    self.u = u
  }

  var u : U
  fileprivate let label = MyLabel()
}

public func generic_class_constrainted_keypath<U, V>(_ c: V) where V : GenericController<U> {
  let kp = \V.label
  print(kp)
  print(c[keyPath: kp].text)
}

// CHECK: Swift.KeyPath<main.GenericController<Swift.Int>, main.MyLabel>
// CHECK: label
generic_class_constrainted_keypath(GenericController(5))
