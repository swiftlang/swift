// RUN: %target-swift-ide-test -print-module -module-to-print=CanonicalTypes -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct __CxxTemplateInst12MagicWrapperI11MagicNumberE {
// CHECK:  var t: MagicNumber
// CHECK:  init()
// CHECK:  init(t: MagicNumber)
// CHECK:  mutating func callGetInt() -> Int32
// CHECK: }
// CHECK: struct MagicNumber {
// CHECK:   init()
// CHECK:   mutating func getInt() -> Int32
// CHECK: }
// CHECK: typealias WrappedMagicNumberA = __CxxTemplateInst12MagicWrapperI11MagicNumberE
// CHECK: typealias WrappedMagicNumberB = __CxxTemplateInst12MagicWrapperI11MagicNumberE
