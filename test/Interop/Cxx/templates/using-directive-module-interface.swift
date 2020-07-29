// RUN: %target-swift-ide-test -print-module -module-to-print=UsingDirective -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct __CxxTemplateInst12MagicWrapperI10IntWrapperE {
// CHECK:   var t: IntWrapper
// CHECK:   init()
// CHECK:   init(t: IntWrapper)
// CHECK:   mutating func getValuePlusArg(_ arg: Int32) -> Int32
// CHECK: }
// CHECK: struct IntWrapper {
// CHECK:   var value: Int32
// CHECK:   init()
// CHECK:   init(value: Int32)
// CHECK:   mutating func getValue() -> Int32
// CHECK: }
// CHECK: typealias UsingWrappedMagicNumber = __CxxTemplateInst12MagicWrapperI10IntWrapperE
