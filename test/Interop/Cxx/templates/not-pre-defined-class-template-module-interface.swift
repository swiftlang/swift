// RUN: %target-swift-ide-test -print-module -module-to-print=NotPreDefinedClassTemplate -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct __CxxTemplateInst12MagicWrapperI10IntWrapperE {
// CHECK-NEXT:   var t: IntWrapper
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(t: IntWrapper)
// CHECK-NEXT:   func getValuePlusArg(_ arg: Int32) -> Int32
// CHECK-NEXT: }
// CHECK-NEXT: struct IntWrapper {
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(value: Int32)
// CHECK-NEXT:   func getValue() -> Int32
// CHECK-NEXT: }
// CHECK-NEXT: typealias MagicallyWrappedIntWithoutDefinition = __CxxTemplateInst12MagicWrapperI10IntWrapperE
