// RUN: %target-run-simple-swift | FileCheck %s
// rdar://16726530

import Foundation

// Test overlain variadic methods.
let expression = NSExpression(format: "(3 + 2)**2", "LLLL", "BBBB")
let result = expression.expressionValueWithObject(nil, context:nil) as NSNumber!
let number = result.stringValue
println(number)

// CHECK: 25
