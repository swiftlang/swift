// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateWithPrimitiveArgument -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: @available(*, unavailable
// CHECK: struct MagicWrapper<T> {
// CHECK: }

// CHECK: struct DoubleWrapper<M> {
// CHECK: }

// CHECK: typealias WrappedMagicInt = MagicWrapper<CInt>
// CHECK: typealias WrappedMagicIntConst = MagicWrapper<CInt_const>
// CHECK: typealias WrappedMagicLongConst = MagicWrapper<CLong_const>
// CHECK: typealias WrappedMagicIntPtr = MagicWrapper<UnsafeMutablePointer<CInt>>
// CHECK: typealias WrappedMagicIntConstPtr = MagicWrapper<UnsafePointer<CInt>>
// CHECK: typealias WrappedMagicIntPtrPtr = MagicWrapper<UnsafeMutablePointer<UnsafeMutablePointer<CInt>>>

// CHECK: typealias DoubleWrappedInt = DoubleWrapper<MagicWrapper<CInt>>
// CHECK: typealias DoubleWrappedIntConst = DoubleWrapper<MagicWrapper<CInt_const>>
// CHECK: typealias DoubleWrappedLongConst = DoubleWrapper<MagicWrapper<CLong_const>>
// CHECK: typealias DoubleWrappedIntPtr = DoubleWrapper<MagicWrapper<UnsafeMutablePointer<CInt>>>
// CHECK: typealias DoubleWrappedIntConstPtr = DoubleWrapper<MagicWrapper<UnsafePointer<CInt>>>
