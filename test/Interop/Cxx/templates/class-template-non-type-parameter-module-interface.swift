// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateNonTypeParameter -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct MagicArray<T, Size> {
// CHECK: }

// CHECK: struct MagicArray<CInt, _C{{.*}}_2> {
// CHECK:   init(t: (CInt, CInt))
// CHECK:   init()
// CHECK:   var t: (CInt, CInt)
// CHECK: }

// CHECK: struct MagicArray<CInt, _C{{.*}}_3> {
// CHECK:   init(t: (CInt, CInt, CInt))
// CHECK:   init()
// CHECK:   var t: (CInt, CInt, CInt)
// CHECK: }

// CHECK: typealias MagicIntPair = MagicArray<CInt, _C{{.*}}_2>
// CHECK: typealias MagicIntTriple = MagicArray<CInt, _C{{.*}}_3>
// CHECK: typealias NegativeThree = integral_constant<CInt, _C{{.*}}_Neg_3>
