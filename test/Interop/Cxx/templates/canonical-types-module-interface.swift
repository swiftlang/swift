// RUN: %target-swift-ide-test -print-module -module-to-print=CanonicalTypes -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct MagicWrapper<IntWrapper> {
// CHECK-NEXT:   init(t: IntWrapper)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var t: IntWrapper
// CHECK-NEXT:   func getValuePlusArg(_ arg: CInt) -> CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct IntWrapper {
// CHECK-NEXT:   init(value: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT:   func getValue() -> CInt
// CHECK-NEXT: }
// CHECK-NEXT: typealias WrappedMagicNumberA = MagicWrapper<IntWrapper>
// CHECK-NEXT: typealias WrappedMagicNumberB = MagicWrapper<IntWrapper>
