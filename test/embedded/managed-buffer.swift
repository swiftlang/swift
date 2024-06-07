// RUN: %target-swift-emit-ir %s -module-name=main -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

// CHECK: @"$s4main8MyBufferCN" = {{.*global.*}} <{ ptr @"$ss13ManagedBufferCySis5UInt8VGN", ptr @"$s4main8MyBufferCfD{{[^"]*}}", ptr null, ptr @"$s4main8MyBufferC12_doNotCallMeACyt_tcfC{{[^"]*}}" }>
// CHECK: @"$ss13ManagedBufferCySis5UInt8VGN" = {{.*global.*}} <{ ptr null, ptr @"$ss13ManagedBufferCfDSi_s5UInt8VTg5{{[^"]*}}", ptr null, ptr @"$ss13ManagedBufferC12_doNotCallMeAByxq_Gyt_tcfCSi_s5UInt8VTg5{{[^"]*}}" }>
final public class MyBuffer: ManagedBuffer<Int, UInt8> {
}

