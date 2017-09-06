// RUN: %target-swift-frontend  -parse-as-library -emit-silgen -enable-sil-ownership %s -verify | %FileCheck %s
// RUN: %target-swift-frontend  -parse-as-library -emit-ir -enable-sil-ownership %s

func function() {
  return

  class UnreachableClass {} // expected-warning {{code after 'return' will never be executed}}
}

// CHECK-LABEL: sil_vtable UnreachableClass
