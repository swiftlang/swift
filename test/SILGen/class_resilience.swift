
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name class_resilience -I %t -enable-library-evolution %s | %FileCheck %s

import resilient_class

// Accessing final property of resilient class from different resilience domain
// through accessor

// CHECK-LABEL: sil [ossa] @$s16class_resilience20finalPropertyOfOtheryy010resilient_A022ResilientOutsideParentCF
// CHECK: function_ref @$s15resilient_class22ResilientOutsideParentC13finalPropertySSvg

public func finalPropertyOfOther(_ other: ResilientOutsideParent) {
  _ = other.finalProperty
}

public class MyResilientClass {
  public final var finalProperty: String = "MyResilientClass.finalProperty"
}

// Accessing final property of resilient class from my resilience domain
// directly

// CHECK-LABEL: sil [ossa] @$s16class_resilience19finalPropertyOfMineyyAA16MyResilientClassCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $MyResilientClass):
// CHECK:   ref_element_addr [[ARG]] : $MyResilientClass, #MyResilientClass.finalProperty
// CHECK: } // end sil function '$s16class_resilience19finalPropertyOfMineyyAA16MyResilientClassCF'

public func finalPropertyOfMine(_ other: MyResilientClass) {
  _ = other.finalProperty
}

class SubclassOfOutsideChild : ResilientOutsideChild {
  override func method() {}

  func newMethod() {}
}

// Note: no entries for [inherited] methods

// CHECK-LABEL: sil_vtable SubclassOfOutsideChild {
// CHECK-NEXT:  #ResilientOutsideParent.init!allocator: (ResilientOutsideParent.Type) -> () -> ResilientOutsideParent : @$s16class_resilience22SubclassOfOutsideChildCACycfC [override]
// CHECK-NEXT:  #ResilientOutsideParent.method: (ResilientOutsideParent) -> () -> () : @$s16class_resilience22SubclassOfOutsideChildC6methodyyF [override]
// CHECK-NEXT:  #SubclassOfOutsideChild.newMethod: (SubclassOfOutsideChild) -> () -> () : @$s16class_resilience22SubclassOfOutsideChildC9newMethodyyF
// CHECK-NEXT:  #SubclassOfOutsideChild.deinit!deallocator: @$s16class_resilience22SubclassOfOutsideChildCfD
// CHECK-NEXT: }
