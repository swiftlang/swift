// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

if true as BooleanType {
  print("true")
}
// CHECK: true
