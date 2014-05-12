// RUN: %target-run-simple-swift | FileCheck %s

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
#if os(OSX)
  println(x.description!)
#else
  println(x.description!())
#endif
}

// CHECK: two
// CHECK: 12
// CHECK: (
// CHECK:   11,
// CHECK:   12,
// CHECK:   13
// CHECK: )
for x: AnyObject in a_m {
#if os(OSX)
  println(x.description!)
#else
  println(x.description!())
#endif
}

class Canary {
  var x: Int

  deinit { println("dead \(x)") }
  init(_ x: Int) { self.x = x }
  func chirp() { println("\(x)") }
}

autoreleasepool {
  println("making array")

  var b: NSArray = NSArray.arrayWithObjects(
    [Canary(1), Canary(2), Canary(3)],
    count: 3
  )

  println("iterating array")

  // CHECK: 1
  for x: AnyObject in b {
    (x as Canary)!.chirp()
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
#if os(OSX)
  println("\(key.description!) => \(value.description!)")
#else
  println("\(key.description!()) => \(value.description!())")
#endif
}

// CHECK: 11510 => B's
for (key, value) in d_m {
#if os(OSX)
  println("\(key.description!) => \(value.description!)")
#else
  println("\(key.description!()) => \(value.description!())")
#endif
}

var s = NSSet.setWithObject("the most forward-thinking test yet")
var s_m = NSMutableSet.setWithObject("the next most forward-thinking test yet")

// CHECK: the most forward-thinking test yet
for x: AnyObject in s! { // FIXME: shouldn't need ! here
#if os(OSX)
  println(x.description!)
#else
  println(x.description!())
#endif
}

// CHECK: the next most forward-thinking test yet
for x: AnyObject in s_m! { // FIXME: shouldn't need ! here
#if os(OSX)
  println(x.description!)
#else
  println(x.description!())
#endif
}

// Enumeration over an NSSwiftArray
// CHECK: hello
// CHECK: bridged
// CHECK: array
var a2 = ["hello", "bridged", "array"]
var nsa2 : NSArray = (a2.asCocoaArray() as AnyObject)!
for x: AnyObject in nsa2 {
#if os(OSX)
  println(x.description!)
#else
  println(x.description!())
#endif
}
