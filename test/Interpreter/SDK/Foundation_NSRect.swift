// RUN: %target-run-simple-swift | FileCheck %s

// iOS doesn't have NSRect. iOS and OS X CGRect is tested elsewhere.
// REQUIRES: OS=macosx

import Foundation

func printRect(r: NSRect) {
  // FIXME: Constraint checker takes too long to typecheck this as an
  // interpolation expression
  print("NSRect(")
  print(r.origin.x)
  print(", ")
  print(r.origin.y)
  print(", ")
  print(r.size.width)
  print(", ")
  print(r.size.height)
  println(")")
}

var r = NSRect(x: 0, y: 0, width: 100, height: 50)

// CHECK: NSRect(20.0, 10.0, 60.0, 30.0)
printRect(NSInsetRect(r, 20, 10))

// CHECK: NSRect(100.0, 100.0, 50.0, 50.0)
printRect(NSMakeRect(100,100,50,50))

// CHECK: {0, 0}, {100, 50}
println(NSStringFromRect(r))
