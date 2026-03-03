// RUN: %target-swift-frontend -module-name test -primary-file %s %S/Inputs/mandatory_inlining_devirt_other.swift -Xllvm -sil-print-types -emit-sil -o - | %FileCheck %s

// rdar://45110471

// We only generate SIL v-tables for these two classes in this file, but
// class hierarchy analysis needs to walk through the third class in the
// helper file.

class Base {
  fileprivate func foo() { print("base") }
  func callit() { foo() }
}

class Derived : Middle {
  fileprivate override func foo() { print("derived") }
}

// CHECK-LABEL: sil hidden @$s4test4BaseC6callityyF
// CHECK:         [[T0:%.*]] = class_method %0 : $Base, #Base.foo
// CHECK-NEXT:    apply [[T0]](%0)
// CHECK-LABEL: } // end sil function '$s4test4BaseC6callityyF'
