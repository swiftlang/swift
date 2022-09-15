// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | grep 'check-prefix' > %t/prefix-option
// RUN: %target-run %t/a.out | %FileCheck `cat %t/prefix-option` %s

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
// CHECK-58: label
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

if #available(SwiftStdlib 5.8, *) {
  print("-check-prefix=CHECK-58")
} else {
  print("-check-prefix=CHECK")
}

// CHECK: Swift.KeyPath<main.GenericController<Swift.Int>, main.MyLabel>
// CHECK: label
// CHECK-58: GenericController<Int>.label
// CHECK-58: label
generic_class_constrained_keypath(GenericController(5))

// CHECK-58: {{\\Controller\.secondLabel!\.text|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>!\.<computed 0x.* \(String\)>}}
print(\Controller.secondLabel!.text)

// CHECK-58: {{\\Controller\.subscript\(_: String\)|\\Controller\.<computed 0x.* \(String\)>}}
print(\Controller["abc"])
// CHECK-58: \S.a
print(\S.a)
// CHECK-58: {{\\Controller\.subscript\(int: Int, str: String, _: Int\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller[int: 0, str: "", 0])
// CHECK-58: {{\\Controller\.thirdLabel|\\Controller\.<computed 0x.* \(Optional<MyLabel>\)>}}
print(\Controller.thirdLabel)
// CHECK-58: {{\\Controller\.subscript\(\)|\\Controller\.<computed 0x.* \(Int\)>}}
print(\Controller.[])
