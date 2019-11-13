// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -module-name A -I %t  %S/Inputs/metadata2.swift -primary-file %s -emit-ir | %FileCheck %s

// CHECK-LABEL: define {{.*}}swiftcc %swift.metadata_response @"$s1A12MyControllerCMr"(%swift.type*, i8*, i8**)
// CHECK-NOT: ret
// CHECK:  call swiftcc %swift.metadata_response @"$s1A17InternalContainerVMa"(
// CHECK:  ret
class MyController {
  var c = InternalContainer(item: [])
  var c2 = InternalContainer2(item: [])
  var e = InternalSingletonEnum()
  var e2 = InternalSingletonEnum2()
  func update(_ n: InternalContainer) {
    c = n
  }
}
