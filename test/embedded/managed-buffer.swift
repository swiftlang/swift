// RUN: %target-swift-emit-ir %s -module-name=main -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: @"$e4main8MyBufferCN" = {{.*global.*}} <{ ptr @"$es13ManagedBufferCySis5UInt8VGN", ptr @"$e4main8MyBufferCfD{{[^"]*}}", ptr null, ptr @"$e4main8MyBufferC12_doNotCallMeACyt_tcfC{{[^"]*}}" }>
// CHECK: @"$es13ManagedBufferCySis5UInt8VGN" = {{.*global.*}} <{ ptr null, ptr @"$es13ManagedBufferCfDSi_s5UInt8VTg5{{[^"]*}}", ptr null, ptr @"$es13ManagedBufferC12_doNotCallMeAByxq_Gyt_tcfCSi_s5UInt8VTg5{{[^"]*}}" }>
final public class MyBuffer: ManagedBuffer<Int, UInt8> {
}

