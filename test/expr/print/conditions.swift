// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

if (5 + 5) == 10 {
}
// CHECK: if (5 + 5) == 10 {
// CHECK: }

if (5 + 5) == 9 {
} else if (5 + 5) == 10 {
} else {
}
// CHECK: if (5 + 5) == 9 {
// CHECK: } else if (5 + 5) == 10 {
// CHECK: } else {
// CHECK: }

guard (5 + 5) == 10 else {
}
// CHECK: guard (5 + 5) == 10 else {
// CHECK: }

var a = 0
// CHECK: @_hasInitialValue internal var a: Int = 0
// Note: the AST doesn't store whitespace,
// so the output doesn't always match the input.
while a < 10 { a += 1 }
// CHECK: while a < 10 {
// CHECK:   a += 1
// CHECK: }

var b = 0
repeat {
  b += 1
} while b < 10
// CHECK: @_hasInitialValue internal var b: Int = 0
// CHECK: repeat {
// CHECK:   b += 1
// CHECK: } while b < 10

var p = (17 > 7 ? true : false)
// CHECK: @_hasInitialValue internal var p: Bool = (17 > 7 ? true : false)

var x: Int = 3
var y: Bool = x is Int
// CHECK: @_hasInitialValue internal var y: Bool = x is Int

enum SomeError: Error {
  case errorType
}

func someThrowingFunc() throws -> SomeError {
  throw SomeError.errorType
}

var tryExpr = try? someThrowingFunc()
// CHECK: @_hasInitialValue internal var tryExpr: SomeError? = try? someThrowingFunc()

var tryForceExpr = try! someThrowingFunc()
