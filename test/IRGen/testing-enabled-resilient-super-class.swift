// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-testing -emit-ir -enable-library-evolution -module-name=resilient %S/Inputs/resilient-class.swift | %FileCheck %s

@testable import resilient

// CHECK: s9resilient8SubClassCMo

public func testCase() {
  let t = SubClass()
  print(t.y)
}
