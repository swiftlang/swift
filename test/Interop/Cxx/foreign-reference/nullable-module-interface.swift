// RUN: %target-swift-ide-test -print-module -module-to-print=Nullable -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: class Empty {
// CHECK:   func test() -> CInt
// CHECK:   class func create() -> Empty!
// CHECK: }
// CHECK: func mutateIt(_: Empty)

// CHECK: class IntPair {
// CHECK:   var a: CInt
// CHECK:   var b: CInt
// CHECK:   func test() -> CInt
// CHECK:   class func create() -> IntPair!
// CHECK: }
// CHECK: func mutateIt(_ x: IntPair!)
