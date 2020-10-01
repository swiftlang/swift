// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/property_wrappers_A.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/Inputs/property_wrappers_B.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t %s
import property_wrappers_A
import property_wrappers_B

struct Test {
  @Wrapper var x: Int = 17
}
