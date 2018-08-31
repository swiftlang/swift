// RUN: %target-run-simple-swift %s | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

// Category on a nested class
class OuterClass {
  class InnerClass: NSObject {}
}

extension OuterClass.InnerClass {
  @objc static let propertyInExtension = "foo"

  @objc func dynamicMethod() -> String {
    return "bar"
  }
}

let x = OuterClass.InnerClass()

// CHECK: foo
print(type(of: x).propertyInExtension)

// CHECK: bar
print(x.dynamicMethod())

// Category on a concrete subclass of a generic base class
class Base<T> {
  let t: T

  init(t: T) { self.t = t }
}

class Derived : Base<Int> {}

extension Derived {
  @objc func otherMethod() -> Int {
    return t
  }
}

let y: AnyObject = Derived(t: 100)

// CHECK: 100
print(y.otherMethod())

extension NSObject {
  @objc func sillyMethod() -> Int {
    return 123
  }
}

let z: AnyObject = NSObject()

// CHECK: 123
print(z.sillyMethod())
