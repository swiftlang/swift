// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print=ClassTemplateWithPrimitiveArgument -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: @available(*, unavailable
// CHECK: struct MagicWrapper<T> {
// CHECK: }

// CHECK: struct DoubleWrapper<M> {
// CHECK: }

// CHECK: typealias WrappedMagicInt = MagicWrapper<CInt>
// CHECK: typealias WrappedMagicIntConst = MagicWrapper<__cxxConst<CInt>>
// CHECK: typealias WrappedMagicLongConst = MagicWrapper<__cxxConst<CLong>>
// CHECK: typealias WrappedMagicIntPtr = MagicWrapper<UnsafeMutablePointer<CInt>>
// CHECK: typealias WrappedMagicIntConstPtr = MagicWrapper<UnsafePointer<CInt>>
// CHECK: typealias WrappedMagicIntPtrPtr = MagicWrapper<UnsafeMutablePointer<UnsafeMutablePointer<CInt>>>
// CHECK: typealias WrappedMagicIntArr = MagicWrapper<[CInt]>
// CHECK: typealias WrappedMagicLongArr = MagicWrapper<[CLong]>
// CHECK: typealias WrappedMagicIntFixedSizeArr1 = MagicWrapper<Vector<CInt, 123>>
// CHECK: typealias WrappedMagicIntFixedSizeArr2 = MagicWrapper<Vector<CInt, 124>>
// CHECK: typealias WrappedMagicNullPtr = MagicWrapper<__cxxNullPtrT>

// CHECK: typealias DoubleWrappedInt = DoubleWrapper<MagicWrapper<CInt>>
// CHECK: typealias DoubleWrappedIntConst = DoubleWrapper<MagicWrapper<__cxxConst<CInt>>>
// CHECK: typealias DoubleWrappedLongConst = DoubleWrapper<MagicWrapper<__cxxConst<CLong>>>
// CHECK: typealias DoubleWrappedIntPtr = DoubleWrapper<MagicWrapper<UnsafeMutablePointer<CInt>>>
// CHECK: typealias DoubleWrappedIntConstPtr = DoubleWrapper<MagicWrapper<UnsafePointer<CInt>>>
// CHECK: typealias DoubleWrappedMagicIntArr = DoubleWrapper<MagicWrapper<[CInt]>>
// CHECK: typealias DoubleWrappedMagicLongArr = DoubleWrapper<MagicWrapper<[CLong]>>
// CHECK: typealias DoubleWrappedMagicIntFixedSizeArr1 = DoubleWrapper<MagicWrapper<Vector<CInt, 42>>>
// CHECK: typealias DoubleWrappedMagicIntFixedSizeArr2 = DoubleWrapper<MagicWrapper<Vector<CInt, 43>>>
// CHECK: typealias DoubleWrappedMagicNullPtr = DoubleWrapper<MagicWrapper<__cxxNullPtrT>>

// CHECK: typealias DoubleConstWrappedInt = DoubleWrapper<__cxxConst<MagicWrapper<CInt>>>
// CHECK: typealias DoubleConstWrappedIntConst = DoubleWrapper<__cxxConst<MagicWrapper<__cxxConst<CInt>>>>
// CHECK: typealias DoubleConstWrappedLongConst = DoubleWrapper<__cxxConst<MagicWrapper<__cxxConst<CLong>>>>
// CHECK: typealias DoubleConstWrappedIntPtr = DoubleWrapper<__cxxConst<MagicWrapper<UnsafeMutablePointer<CInt>>>>
// CHECK: typealias DoubleConstWrappedIntConstPtr = DoubleWrapper<__cxxConst<MagicWrapper<UnsafePointer<CInt>>>>
// CHECK: typealias DoubleConstWrappedMagicIntArr = DoubleWrapper<__cxxConst<MagicWrapper<[CInt]>>>
// CHECK: typealias DoubleConstWrappedMagicLongArr = DoubleWrapper<__cxxConst<MagicWrapper<[CLong]>>>
// CHECK: typealias DoubleConstWrappedMagicIntFixedSizeArr1 = DoubleWrapper<__cxxConst<MagicWrapper<Vector<CInt, 42>>>>
// CHECK: typealias DoubleConstWrappedMagicIntFixedSizeArr2 = DoubleWrapper<__cxxConst<MagicWrapper<Vector<CInt, 43>>>>
// CHECK: typealias DoubleConstWrappedMagicNullPtr = DoubleWrapper<__cxxConst<MagicWrapper<__cxxNullPtrT>>>

// CHECK: typealias WrappedVolatileInt = MagicWrapper<__cxxVolatile<CInt>>
// CHECK: typealias WrappedConstVolatileInt = MagicWrapper<__cxxConst<__cxxVolatile<CInt>>>
// CHECK: typealias WrappedVolatileConstInt = MagicWrapper<__cxxConst<__cxxVolatile<CInt>>>
