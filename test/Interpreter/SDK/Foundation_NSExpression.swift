// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// rdar://16726530

// REQUIRES: objc_interop


import Foundation

// Test overlain variadic methods.
let expression = NSExpression(format: "(3 + 2)**2", "LLLL", "BBBB")
let result = expression.expressionValueWithObject(expression, context:nil) as! NSNumber
let number = result.stringValue
print(number)

// CHECK: 25
