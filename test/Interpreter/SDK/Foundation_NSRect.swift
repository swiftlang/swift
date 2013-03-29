// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -i %s | FileCheck %s
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%sdk -sil-irgen -i %s | FileCheck %s
// REQUIRES: sdk

import Foundation

func printRect(r:NSRect) {
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

var r = NSRect(0, 0, 100, 50)

// CHECK: NSRect(20.0, 10.0, 60.0, 30.0)
printRect(NSInsetRect(r, 20, 10))
