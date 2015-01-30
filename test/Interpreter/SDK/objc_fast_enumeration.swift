// RUN: %target-run-simple-swift | FileCheck %s

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
for x: AnyObject in a {
  println(x.description!)
}

// CHECK: two
// CHECK: 12
// CHECK: (
// CHECK:   11,
// CHECK:   12,
// CHECK:   13
// CHECK: )
for x: AnyObject in a_m {
  println(x.description!)
}

class Canary {
  var x: Int

  deinit { println("dead \(x)") }
  init(_ x: Int) { self.x = x }
  func chirp() { println("\(x)") }
}

autoreleasepool {
  println("making array")

  var b: NSArray = NSArray(objects: [Canary(1), Canary(2), Canary(3)], count: 3)

  println("iterating array")

  // CHECK: 1
  for x: AnyObject in b {
    (x as! Canary).chirp()
    break
  }

  // CHECK: exiting
  println("exiting")
}
// CHECK: dead
// CHECK: dead
// CHECK: dead
// CHECK: exited
println("exited")

var d : NSDictionary = [415 : "Giants", 510 : "A's"]
var d_m : NSMutableDictionary = [1415 : "Big Giants", 11510 : "B's"]

// CHECK: 510 => A's
for (key, value) in d {
  println("\(key.description!) => \(value.description!)")
}

// CHECK: 11510 => B's
for (key, value) in d_m {
  println("\(key.description!) => \(value.description!)")
}

var s = NSSet(object: "the most forward-thinking test yet")
var s_m = NSMutableSet(object: "the next most forward-thinking test yet")

// CHECK: the most forward-thinking test yet
for x: AnyObject in s {
  println(x.description!)
}

// CHECK: the next most forward-thinking test yet
for x: AnyObject in s_m {
  println(x.description!)
}

// Enumeration over a _SwiftDeferredNSArray
// CHECK: 3
// CHECK: 2
// CHECK: 1
var a2 = [3, 2, 1]
var nsa2 = (a2._buffer._asCocoaArray() as AnyObject) as! NSArray
for x: AnyObject in nsa2 {
  println(x.description!)
}
