// RUN: %target-run-simple-swift | FileCheck %s

if true as BooleanType {
  println("true")
}
// CHECK: true