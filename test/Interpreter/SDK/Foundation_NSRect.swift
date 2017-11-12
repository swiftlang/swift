// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// iOS doesn't have NSRect. iOS and OS X CGRect is tested elsewhere.
// REQUIRES: OS=macosx

import Foundation

func printRect(_ r: NSRect) {
  // FIXME: Constraint checker takes too long to typecheck this as an
  // interpolation expression
  print("NSRect(", terminator: "")
  print(r.origin.x, terminator: "")
  print(", ", terminator: "")
  print(r.origin.y, terminator: "")
  print(", ", terminator: "")
  print(r.size.width, terminator: "")
  print(", ", terminator: "")
  print(r.size.height, terminator: "")
  print(")")
}

var r = NSRect(x: 0, y: 0, width: 100, height: 50)

// CHECK: NSRect(20.0, 10.0, 60.0, 30.0)
printRect(NSInsetRect(r, 20, 10))

// CHECK: NSRect(100.0, 100.0, 50.0, 50.0)
printRect(NSMakeRect(100,100,50,50))

// CHECK: {0, 0}, {100, 50}
print(NSStringFromRect(r))

// CHECK: NSRect(1.5, 1.5, 1.5, 1.5)
let d = 1.5
var r2 = NSRect(x: d, y: d, width: d, height: d)
printRect(r2)
