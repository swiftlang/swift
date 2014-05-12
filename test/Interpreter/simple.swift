// RUN: %target-run-simple-swift | FileCheck %s

// CHECK: 123ABCD
// CHECK: Hello ☃
// CHECK: Hello ☃

if (true) {
  print(123)
  print(UnicodeScalar(65))
  print(UnicodeScalar(66))
  print(UnicodeScalar(67))
  print(UnicodeScalar(0o104))
  print(UnicodeScalar(10))
  print("Hello \u2603\n")  // Hi Snowman!
  print("Hello ☃\n")
}
