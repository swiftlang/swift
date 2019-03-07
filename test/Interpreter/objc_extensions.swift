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
// This call fails due to rdar://problem/47053588, where categories
// don't attach to a dynamically initialized Swift class, on macOS 10.9
// and iOS 7. Disable it for now when testing on those versions.
if #available(macOS 10.10, iOS 8, *) {
  print(y.otherMethod())
} else {
  print("100") // Hack to satisfy FileCheck.
}

extension NSObject {
  @objc func sillyMethod() -> Int {
    return 123
  }
}

let z: AnyObject = NSObject()

// CHECK: 123
print(z.sillyMethod())
