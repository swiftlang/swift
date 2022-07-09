// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name A -emit-module-path %t/A.swiftmodule %S/Inputs/prespecialize_import_module.swift
// RUN: %target-swift-frontend -O -emit-sil -module-name B  -I %t %s | %FileCheck %s
import A

// CHECK-LABEL: sil{{.*}} @$s1B4testyyF
public func test() {
  // CHECK: s1A8someFuncyyxlFSi_Ts5
  someFunc(5)
}
// CHECK: end sil function '$s1B4testyyF'
