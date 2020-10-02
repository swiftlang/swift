// RUN: %target-swift-ide-test -print-module -module-to-print=Reference -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: func getStaticInt() -> Int32
// CHECK: func getStaticIntRef() -> UnsafeMutablePointer<Int32>
// CHECK: func getStaticIntRvalueRef() -> UnsafeMutablePointer<Int32>
// CHECK: func getConstStaticIntRef() -> UnsafePointer<Int32>
// CHECK: func getConstStaticIntRvalueRef() -> UnsafePointer<Int32>
// CHECK: func setStaticInt(_: Int32)
// CHECK: func setStaticIntRef(_: UnsafeMutablePointer<Int32>)
// CHECK: func setStaticIntRvalueRef(_: UnsafeMutablePointer<Int32>)
// CHECK: func setConstStaticIntRef(_: UnsafePointer<Int32>)
// CHECK: func setConstStaticIntRvalueRef(_: UnsafePointer<Int32>)
// CHECK: func getFuncRef() -> @convention(c) () -> Int32
// CHECK: func getFuncRvalueRef() -> @convention(c) () -> Int32
