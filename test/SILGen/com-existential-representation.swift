// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -parse-as-library -module-name M -emit-silgen -enable-experimental-com-interop -I %t %s | %FileCheck %s

// REQUIRES: PTRSIZE=64

// A COM existential is a loadable, nontrivial value. Copying and destroying it
// must remain abstract SIL value operations so IRGen can use the COM vtable
// instead of Swift ARC.

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget { }

// CHECK-LABEL: sil [ossa] @$s{{.*}}4copyyAA7IWidget_pAaC_pF
// CHECK-SAME:  @guaranteed any IWidget
// CHECK-SAME:  @owned any IWidget
// CHECK:         explicit_copy_value
// CHECK-NOT:     strong_retain
// CHECK:         return
public func copy(_ value: borrowing any IWidget) -> any IWidget {
  copy value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}7consumeyyAA7IWidget_pnF
// CHECK-SAME:  @owned any IWidget
// CHECK-NOT:     strong_release
// CHECK:         destroy_value
// CHECK:         return
public func consume(_ value: consuming any IWidget) { }

public struct Holder {
  public var value: any IWidget

  // CHECK-LABEL: sil [ossa] @$s{{.*}}6HolderV{{.*}}cfC
  // CHECK-SAME:  @owned any IWidget
  // CHECK-NOT:   init_existential
  // CHECK:       return
  public init(_ value: consuming any IWidget) {
    self.value = value
  }
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4copy8optionalAA7IWidget_pSgA{{.*}}F
// CHECK-SAME:  @guaranteed Optional<any IWidget>
// CHECK-SAME:  @owned Optional<any IWidget>
// CHECK:       explicit_copy_value
// CHECK-NOT:   strong_retain
// CHECK:       return
public func copy(optional value: borrowing (any IWidget)?) -> (any IWidget)? {
  copy value
}

// CHECK-LABEL: sil [ossa] @$s{{.*}}4make5arraySayAA7IWidget_pGAaD_pn_tF
// CHECK-SAME:  @owned any IWidget
// CHECK-NOT:   init_existential
// CHECK:       return
public func make(array value: consuming any IWidget) -> [any IWidget] {
  [value]
}
