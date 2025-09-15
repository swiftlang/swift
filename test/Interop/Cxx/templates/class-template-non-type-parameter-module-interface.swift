// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateNonTypeParameter -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct MagicArray<T, Size> {
// CHECK: }

// CHECK: struct MagicArray<CInt, _C{{.*}}_2> {
// CHECK:   init()
// CHECK:   init(t: (Int32, Int32))
// CHECK:   var t: (Int32, Int32)
// CHECK: }

// CHECK: struct MagicArray<CInt, _C{{.*}}_3> {
// CHECK:   init()
// CHECK:   init(t: (Int32, Int32, Int32))
// CHECK:   var t: (Int32, Int32, Int32)
// CHECK: }

// CHECK: typealias MagicIntPair = MagicArray<CInt, _C{{.*}}_2>
// CHECK: typealias MagicIntTriple = MagicArray<CInt, _C{{.*}}_3>
// CHECK: typealias NegativeThree = integral_constant<CInt, _C{{.*}}_Neg_3>
