// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK: 123ABCD
// CHECK: Hello ☃
// CHECK: Hello ☃

if (true) {
  print(123, terminator: "")
  print(UnicodeScalar(65)!, terminator: "")
  print(UnicodeScalar(66)!, terminator: "")
  print(UnicodeScalar(67)!, terminator: "")
  print(UnicodeScalar(0o104)!, terminator: "")
  print(UnicodeScalar(10)!, terminator: "")
  print("Hello \u{2603}\n", terminator: "")  // Hi Snowman!
  print("Hello ☃\n", terminator: "")
}
