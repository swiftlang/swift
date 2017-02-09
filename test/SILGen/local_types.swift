// RUN: %target-swift-frontend  -parse-as-library -emit-silgen %s -verify | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -emit-ir %s

func function() {
  return

  class UnreachableClass {} // expected-warning {{code after 'return' will never be executed}}
}

// CHECK-LABEL: sil_vtable UnreachableClass
