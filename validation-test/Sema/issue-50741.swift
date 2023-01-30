// RUN: %target-swift-frontend -module-name M -primary-file %s %S/Inputs/issue-50741-helper.swift -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name M -primary-file %s %S/Inputs/issue-50741-helper.swift -DREVERSE -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name M %S/Inputs/issue-50741-helper.swift -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name M %S/Inputs/issue-50741-helper.swift -primary-file %s -DREVERSE -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/50741

import Foundation

// CHECK-LABEL: define {{.+}} @"$s1M4test10ObjectiveC8SelectorVyF"() {{#[0-9]+}}
func test() -> Selector {
  // CHECK: = load {{.+}} @"\01L_selector(isAsynchronous)"
  return #selector(getter: AsyncValueBlockOperation.isAsynchronous)
  // CHECK: ret
}
