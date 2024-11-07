// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-ir -primary-file %s %S/Inputs/opaque_result_type_private_typemetadata2.swift | %FileCheck %s

// Container's fields are not ABI accessible so copying Container must use its
// metadata instead of exploding its fields.

// CHECK: define{{.*}} swiftcc void @"$s39opaque_result_type_private_typemetadata4doItyyF"()
// CHECK-NOT:  ret void
// CHECK: call {{.*}} @"$s39opaque_result_type_private_typemetadata9ContainerVMa"(
// CHECK:  ret void

public func doIt() {
  var x = Container()
  var y = x
}
