// RUN: %target-swift-frontend -print-ast -enable-experimental-feature ThenStatements %s 2>&1 | %FileCheck %s

// Required for experimental features
// REQUIRES: asserts

func foo() -> Int {
  if .random() {
    then 0
  } else {
    0
  }
  // Make sure we don't print implicit 'then' statements.
  // CHECK:      return if Bool.random() {
  // CHECK-NEXT: {{^}}  then 0
  // CHECK-NEXT: } else {
  // CHECK-NEXT: {{^}}  0
  // CHECK-NEXT: }
}
