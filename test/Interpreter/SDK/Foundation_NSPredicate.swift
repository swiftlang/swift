// RUN: %target-run-simple-swift | FileCheck %s
// rdar://16726530

import Foundation

// Test overlain variadic methods.
let s = NSPredicate(format: "(lastName like[cd] %@) AND (birthday > %@)", "LLLL", "BBBB")
println(s.predicateFormat)

// CHECK: lastName LIKE[cd] "LLLL" AND birthday > "BBBB"
