// RUN: %target-swift-ide-test -print-module -module-to-print=Reference -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func getStaticInt() -> CInt
// CHECK: func getStaticIntRef() -> UnsafeMutablePointer<CInt>
// CHECK: func getStaticIntRvalueRef() -> UnsafeMutablePointer<CInt>
// CHECK: func getConstStaticIntRef() -> UnsafePointer<CInt>
// CHECK: func getConstStaticIntRvalueRef() -> UnsafePointer<CInt>
// CHECK: func setStaticInt(_: CInt)
// CHECK: func setStaticIntRef(_: inout CInt)
// CHECK: func setConstStaticIntRef(_: CInt)
// CHECK: func getFuncRef() -> @convention(c) () -> CInt
// CHECK: func getFuncRvalueRef() -> @convention(c) () -> CInt
// CHECK: func setConstStaticIntRefTypealias(_ ref: CInt)
// CHECK: func setStaticIntRefTypealias(_ ref: inout CInt)
// CHECK: func refToTemplate<T>(_ t: inout T) -> UnsafeMutablePointer<T>
// CHECK: func constRefToTemplate<T>(_ t: T) -> UnsafePointer<T>

// CHECK-NOT: refToDependent
// CHECK-NOT: refToDependentParam
// CHECK-NOT: dontImportAtomicRef
// CHECK-NOT: setStaticIntRvalueRef
// CHECK-NOT: setConstStaticIntRvalueRef
