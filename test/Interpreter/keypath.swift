// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

class MyLabel {
  var text = "label"
}

class Controller {
  
  fileprivate let label = MyLabel()
  fileprivate var secondLabel: MyLabel? = MyLabel()
  public var thirdLabel: MyLabel? = MyLabel()
  
  subscript(string: String) -> String {
    get {
      ""
    }
    set {
      
    }
  }
  
  subscript(int int: Int, str str: String, otherInt: Int) -> Int {
    get {
      0
    }
    set {
      
    }
  }
  
  subscript() -> Int {
    0
  }
  
}

struct S {
  var a: Int
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

public func generic_class_constrained_keypath<U, V>(_ c: V) where V : GenericController<U> {
  let kp = \V.label
  print(kp)
  print(c[keyPath: kp].text)
}

// CHECK: GenericController<Int>.label
// CHECK: label
generic_class_constrained_keypath(GenericController(5))

// CHECK: {{\\Controller\.secondLabel!\.text|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>!\.<computed 0x.* \(String\)>}}
print(\Controller.secondLabel!.text)

// CHECK: {{\\Controller\.subscript\(_: String\)|\\Controller\.<computed 0x.* \(String\)>}}
print(\Controller["abc"])
// CHECK: \S.a
print(\S.a)
// CHECK: {{\\Controller\.subscript\(int: Int, str: String, _: Int\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller[int: 0, str: "", 0])
// CHECK: {{\\Controller\.thirdLabel|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>}}
print(\Controller.thirdLabel)
// CHECK: {{\\Controller\.subscript\(\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller.[])
// CHECK: \Controller.self
print(\Controller.self)
