// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

// CHECK: 123ABCD
// CHECK: Hello ☃
// CHECK: Hello ☃

if (true) {
  print(123, appendNewline: false)
  print(UnicodeScalar(65), appendNewline: false)
  print(UnicodeScalar(66), appendNewline: false)
  print(UnicodeScalar(67), appendNewline: false)
  print(UnicodeScalar(0o104), appendNewline: false)
  print(UnicodeScalar(10), appendNewline: false)
  print("Hello \u{2603}\n", appendNewline: false)  // Hi Snowman!
  print("Hello ☃\n", appendNewline: false)
}
