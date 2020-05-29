// RUN: %target-swift-ide-test -print-module -module-to-print=Typedefs -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct Banana {
// CHECK:   init()
// CHECK:   mutating func peel() -> Int32
// CHECK: }
// CHECK: struct __Peel__Banana__ {
// CHECK:   var t: Banana
// CHECK:   init()
// CHECK:   init(t: Banana)
// CHECK:   mutating func doPeel() -> Int32
// CHECK: }
// CHECK: typealias PeeledBanana = __Peel__Banana__
// CHECK: typealias OtherPeeledBanana = __Peel__Banana__
