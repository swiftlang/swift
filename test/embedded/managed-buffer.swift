// RUN: %target-swift-emit-ir -disable-experimental-feature EmbeddedExistentials %s -module-name=main -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-emit-ir  %s -module-name=main -enable-experimental-feature Embedded | %FileCheck %s --check-prefix=EXIST

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: @"$e4main8MyBufferCN" = {{.*global.*}} <{ ptr @"$es13ManagedBufferCySis5UInt8VGN", ptr @"$e4main8MyBufferCfD{{[^"]*}}", ptr null, ptr @"$e4main8MyBufferC12_doNotCallMeACyt_tcfC{{[^"]*}}" }>
// CHECK: @"$es13ManagedBufferCySis5UInt8VGN" = {{.*global.*}} <{ ptr null, ptr @"$es13ManagedBufferCfDSi_s5UInt8VTgq5{{[^"]*}}", ptr null, ptr @"$es13ManagedBufferC12_doNotCallMeAByxq_Gyt_tcfCSi_s5UInt8VTgq5{{[^"]*}}" }>

// EXIST: @"$e4main8MyBufferCMf" = {{.*}} <{ ptr @"$eBoWV{{[^"]*}}", {{.*}} ptr @"$es13ManagedBufferCySis5UInt8VGMf", i32 0, i32 1), ptr @"$e4main8MyBufferCfD{{[^"]*}}", ptr null, ptr @"$e4main8MyBufferC12_doNotCallMeACyt_tcfC{{[^"]*}}" }>
// EXIST: @"$es13ManagedBufferCySis5UInt8VGMf" = {{.*}} <{ ptr @"$eBoWV{{[^"]*}}", ptr null, ptr @"$es13ManagedBufferCfDSi_s5UInt8VTgq5{{[^"]*}}", ptr null, ptr @"$es13ManagedBufferC12_doNotCallMeAByxq_Gyt_tcfCSi_s5UInt8VTgq5{{[^"]*}}" }>

// EXIST: @"$e4main8MyBufferCN" = {{.*}}alias
// EXIST: @"$es13ManagedBufferCySis5UInt8VGN" = {{.*}}alias

final public class MyBuffer: ManagedBuffer<Int, UInt8> {
}

