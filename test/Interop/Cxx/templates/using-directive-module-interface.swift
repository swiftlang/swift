// RUN: %target-swift-ide-test -print-module -module-to-print=UsingDirective -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct MagicWrapper<IntWrapper> {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(t: IntWrapper)
// CHECK-NEXT:   var t: IntWrapper
// CHECK-NEXT:   func getValuePlusArg(_ arg: Int32) -> Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct IntWrapper {
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(value: Int32)
// CHECK-NEXT:   func getValue() -> Int32
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }
// CHECK-NEXT: typealias UsingWrappedMagicNumber = MagicWrapper<IntWrapper>
