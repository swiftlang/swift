// RUN: %target-swift-emit-ir -I %S/Inputs -enable-experimental-cxx-interop %s -Xcc -fignore-exceptions | %FileCheck %s

import ProtocolConformance

protocol HasReturn42 {
  mutating func return42() -> CInt
}


// CHECK: define {{.*}}i32 @"$sSo18ConformsToProtocolV4main11HasReturn42A2cDP8return42s5Int32VyFTW"(ptr{{( nocapture)?}} swiftself{{( captures\(none\))?}} dereferenceable(1) %{{.*}}, ptr %{{.*}}, ptr %{{.*}})
// CHECK: [[OUT:%.*]] = call i32 @{{_ZN18ConformsToProtocol8return42Ev|"\?return42@ConformsToProtocol@@QEAAHXZ"}}(ptr
// CHECK: ret i32 [[OUT]]

// CHECK: define hidden swiftcc i32 @"$sSo0014SAnon0_wuJCavaV4mainE7method0s5Int32VyF"(
// CHECK: ret i32 123

// CHECK: define internal swiftcc i32 @"$sSo0014SAnon0_wuJCavaV4main1PA2cDP7method0s5Int32VyFTW"(
// CHECK: %[[V1:.*]] = call swiftcc i32 @"$sSo0014SAnon0_wuJCavaV4mainE7method0s5Int32VyF"(
// CHECK: ret i32 %[[V1]]

// CHECK: define hidden swiftcc i32 @"$sSo0014SAnon1_wuJCavaV4mainE7method0s5Int32VyF"(
// CHECK: ret i32 234

// CHECK: define internal swiftcc i32 @"$sSo0014SAnon1_wuJCavaV4main1PA2cDP7method0s5Int32VyFTW"(
// CHECK: %[[V1:.*]] = call swiftcc i32 @"$sSo0014SAnon1_wuJCavaV4mainE7method0s5Int32VyF"(
// CHECK: ret i32 %[[V1]]

// CHECK: define {{.*}}%swift.metadata_response @"$sSo18ConformsToProtocolVMa"({{i64|i32}} [[ARG:%.*]])
// CHECK: load ptr, ptr @"$sSo18ConformsToProtocolVML"
// CHECK: call swiftcc %swift.metadata_response @swift_getForeignTypeMetadata({{i64|i32}} [[ARG]]
// CHECK: ret %swift.metadata_response

extension ConformsToProtocol : HasReturn42 {}

protocol P {
  func method0() -> Int32
}

extension AnonType0 : P {
  func method0() -> Int32 {
    return 123
  }
}

extension AnonType1 : P {
  func method0() -> Int32 {
    return 234
  }
}
