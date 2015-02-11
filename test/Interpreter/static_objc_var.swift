// RUN: %target-run-simple-swift | FileCheck %s

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
println(C.self.respondsToSelector("i"))

// CHECK: 2
println(C.i)

// CHECK: false
println(C.self.respondsToSelector("setI:"))

// CHECK: true
println(C.self.respondsToSelector("j"))

// CHECK: Hello
println(C.j)

C.j = "World"

// CHECK: World
println(C.j)

// CHECK: true
println(C.self.respondsToSelector("setJ:"))

// CHECK: Test
C.performSelectorOnMainThread("setJ:", withObject: "Test", waitUntilDone: true)
println(C.j)

// CHECK: OK
C.j = "OK"
println(C.j)

// CHECK: true
println(C.self.respondsToSelector("k"))

// CHECK: 3.14
println(C.k)

// CHECK: false
println(C.self.respondsToSelector("setK:"))
