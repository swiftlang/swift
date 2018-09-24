// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

var a: NSArray = ["one", 2, [1,2,3]]
var a_m: NSMutableArray = ["two", 12, [11,12,13]]

// CHECK: one
// CHECK: 2
// CHECK: (
// CHECK:   1,
// CHECK:   2,
// CHECK:   3
// CHECK: )
for x in a {
  print(x)
}

// CHECK: two
// CHECK: 12
// CHECK: (
// CHECK:   11,
// CHECK:   12,
// CHECK:   13
// CHECK: )
for x in a_m {
  print(x)
}

class Canary {
  var x: Int

  deinit { print("dead \(x)") }
  init(_ x: Int) { self.x = x }
  func chirp() { print("\(x)") }
}

autoreleasepool {
  print("making array")

  var b: NSArray = NSArray(objects: [Canary(1), Canary(2), Canary(3)], count: 3)

  print("iterating array")

  // CHECK: 1
  for x in b {
    (x as! Canary).chirp()
    break
  }

  // CHECK: exiting
  print("exiting")
}
// CHECK: dead
// CHECK: dead
// CHECK: dead
// CHECK: exited
print("exited")

var d : NSDictionary = [415 : "Giants" , 510 : "A's"]
var d_m : NSMutableDictionary = [1415 : "Big Giants", 11510 : "B's"]

// CHECK: 510 => A's
for (key, value) in d {
  print("\(key) => \(value)")
}

// CHECK: 11510 => B's
for (key, value) in d_m {
  print("\(key) => \(value)")
}

var s = NSSet(object: "the most forward-thinking test yet")
var s_m = NSMutableSet(object: "the next most forward-thinking test yet")

// CHECK: the most forward-thinking test yet
for x in s {
  print(x)
}

// CHECK: the next most forward-thinking test yet
for x in s_m {
  print(x)
}

// Enumeration over a __SwiftDeferredNSArray
// CHECK: 3
// CHECK: 2
// CHECK: 1
var a2 = [3, 2, 1]
var nsa2: NSArray = a2._bridgeToObjectiveC()
for x in nsa2 {
  print(x)
}

class X : CustomStringConvertible {
  init(_ value: Int) { self.value = value }
  var value: Int
  var description: String { return "X(\(value))" }
}

// Enumeration over a _ContiguousArrayBuffer
// CHECK: X(3)
// CHECK: X(2)
// CHECK: X(1)
var a3 = [X(3), X(2), X(1)]
var nsa3: NSArray = a3._bridgeToObjectiveC()
for x in nsa3 {
  print(x)
}
