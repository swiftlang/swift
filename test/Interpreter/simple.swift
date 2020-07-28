// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK: 123ABCD
// CHECK: Hello ☃
// CHECK: Hello ☃

if (true) {
  print(123, terminator: "")
  print(Unicode.Scalar(65)!, terminator: "")
  print(Unicode.Scalar(66)!, terminator: "")
  print(Unicode.Scalar(67)!, terminator: "")
  print(Unicode.Scalar(0o104)!, terminator: "")
  print(Unicode.Scalar(10)!, terminator: "")
  print("Hello \u{2603}\n", terminator: "")  // Hi Snowman!
  print("Hello ☃\n", terminator: "")
}
