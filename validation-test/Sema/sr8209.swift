// RUN: %target-swift-frontend -module-name SR8209 -primary-file %s %S/Inputs/sr8209-helper.swift -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name SR8209 -primary-file %s %S/Inputs/sr8209-helper.swift -DREVERSE -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name SR8209 %S/Inputs/sr8209-helper.swift -primary-file %s -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -module-name SR8209 %S/Inputs/sr8209-helper.swift -primary-file %s -DREVERSE -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: define {{.+}} @"$S6SR82094test10ObjectiveC8SelectorVyF"() {{#[0-9]+}} {
func test() -> Selector {
  // CHECK: = load {{.+}} @"\01L_selector(isAsynchronous)"
  return #selector(getter: AsyncValueBlockOperation.isAsynchronous)
  // CHECK: ret
}
