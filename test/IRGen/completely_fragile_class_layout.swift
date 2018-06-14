// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend  -I %t -emit-ir -enable-resilience %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize
// RUN: %target-swift-frontend  -I %t -emit-ir -enable-resilience -O %s

// REQUIRES: objc_interop

import resilient_struct

// Class has static metadata:
// CHECK-LABEL: $S31completely_fragile_class_layout23ClassWithResilientFieldCMf

// CHECK-LABEL: define swiftcc %swift.metadata_response @"$S31completely_fragile_class_layout23ClassWithResilientFieldCMa
// CHECK: call %objc_class* @swift_getInitializedObjCClass(%objc_class* {{.*}} @"$S31completely_fragile_class_layout23ClassWithResilientFieldCMf"
// CHECK: ret

public class ClassWithResilientField {
  var first: Int
  var second: Size
  var third: Int

  init(x: Int, y: Size, z: Int) {
    self.first = x
    self.second = y
    self.third = z
  }
}
