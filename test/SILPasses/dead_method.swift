// RUN: %target-swift-frontend -O -primary-file %s %S/Inputs/public_class.swift -parse-as-library -emit-sil | FileCheck %s

private class Derived : Base {
  override func visible() {
  }
  func is_dead() {
  }
}

// CHECK: sil private {{.*}}visible
// CHECK-NOT: sil {{.*}}is_dead

// CHECK-LABEL: sil_vtable
// CHECK: Base.visible
// CHECK-NOT: is_dead
