// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-builtin-module -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -enable-experimental-feature AddressableParameters -module-name lifetime_dependence_ref -o %t %S/Inputs/lifetime_dependence.swift
// RUN: %target-swift-frontend -I %t -emit-ir %s -verify | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_AddressableParameters

import lifetime_dependence_ref

public func foo() {
  let x = 123
  let y = Ref(x)

  // CHECK: print
  print(y[])
}
