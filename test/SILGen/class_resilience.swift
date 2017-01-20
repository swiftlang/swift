// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_class.swiftmodule -module-name=resilient_class -I %t %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -I %t -emit-silgen -enable-resilience %s | %FileCheck %s

import resilient_class

// Accessing final property of resilient class from different resilience domain
// through accessor

// CHECK-LABEL: sil @_TF16class_resilience20finalPropertyOfOtherFC15resilient_class22ResilientOutsideParentT_
// CHECK: function_ref @_TFC15resilient_class22ResilientOutsideParentg13finalPropertySS

public func finalPropertyOfOther(_ other: ResilientOutsideParent) {
  _ = other.finalProperty
}

public class MyResilientClass {
  public final var finalProperty: String = "MyResilientClass.finalProperty"
}

// Accessing final property of resilient class from my resilience domain
// directly

// CHECK-LABEL: sil @_TF16class_resilience19finalPropertyOfMineFCS_16MyResilientClassT_
// CHECK: ref_element_addr %0 : $MyResilientClass, #MyResilientClass.finalProperty

public func finalPropertyOfMine(_ other: MyResilientClass) {
  _ = other.finalProperty
}

