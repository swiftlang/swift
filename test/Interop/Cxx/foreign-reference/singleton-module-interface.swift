// RUN: %target-swift-ide-test -print-module -module-to-print=Singleton -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK-NOT: init
// CHECK: class DeletedDtor {
// CHECK:   var value: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> DeletedDtor
// CHECK: }
// CHECK: func mutateIt(_ x: DeletedDtor)

// CHECK: class PrivateDtor {
// CHECK:   var value: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> PrivateDtor
// CHECK: }
// CHECK: func mutateIt(_ x: PrivateDtor)

// CHECK: class DeletedSpecialMembers {
// CHECK:   var value: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> DeletedSpecialMembers
// CHECK: }
// CHECK: func mutateIt(_ x: DeletedSpecialMembers)

// CHECK: class PrivateSpecialMembers {
// CHECK:   var value: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> PrivateSpecialMembers
// CHECK: }
// CHECK: func mutateIt(_ x: PrivateSpecialMembers)
