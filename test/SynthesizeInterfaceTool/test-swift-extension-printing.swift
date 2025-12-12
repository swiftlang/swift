// RUN: %target-swift-synthesize-interface -module-name TestExt -F %S/Inputs/Frameworks -o - | %FileCheck %s

// REQUIRES: OS=macosx && CPU=arm64

// CHECK: open class MyType

// CHECK: extension MyType {
// CHECK:     public struct MyInnerType {
// CHECK:         public func test()
// CHECK:     }
// CHECK: }

// CHECK-NOT: public struct MyInnerType

// CHECK: extension Int {
// CHECK:     public struct OtherInnerType {
// CHECK:     }
// CHECK: }

// CHECK-NOT: public struct MyInnerType
