// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift

// RUN: %target-swift-frontend  -I %t -emit-ir -enable-library-evolution %s -read-legacy-type-info-path=%S/Inputs/legacy_type_info/a.yaml | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize -DINT=i%target-ptrsize

// We only use fragile class layouts when Objective-C interop is enabled.

// REQUIRES: objc_interop
// REQUIRES: CPU=arm64e

import resilient_struct

public class HasResilientField {
  let i: ResilientRef

  init(i: ResilientRef) {
    self.i = i
  }
}

// CHECK-LABEL: @_DATA__TtC4main17HasResilientField = internal constant
// CHECK-SAME: @"$s4main17HasResilientFieldCMU.ptrauth"
