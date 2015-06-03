// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop


import Foundation

class C : NSObject {
  static let i = 2
  static var j = "Hello"
  static var k: Double {
    return 3.14
  }
}

// CHECK: true
print(C.self.respondsToSelector("i"))

// CHECK: 2
print(C.i)

// CHECK: false
print(C.self.respondsToSelector("setI:"))

// CHECK: true
print(C.self.respondsToSelector("j"))

// CHECK: Hello
print(C.j)

C.j = "World"

// CHECK: World
print(C.j)

// CHECK: true
print(C.self.respondsToSelector("setJ:"))

// CHECK: Test
C.performSelectorOnMainThread("setJ:", withObject: "Test", waitUntilDone: true)
print(C.j)

// CHECK: OK
C.j = "OK"
print(C.j)

// CHECK: true
print(C.self.respondsToSelector("k"))

// CHECK: 3.14
print(C.k)

// CHECK: false
print(C.self.respondsToSelector("setK:"))
