// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

if true as Boolean {
  print("true")
}
// CHECK: true
