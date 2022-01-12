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
