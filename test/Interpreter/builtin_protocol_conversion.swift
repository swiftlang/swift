// RUN: %target-run-simple-swift | FileCheck %s

if true as BooleanType {
  print("true")
}
// CHECK: true
