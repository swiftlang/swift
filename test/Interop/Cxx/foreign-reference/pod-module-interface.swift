// RUN: %target-swift-ide-test -print-module -module-to-print=POD -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: class Empty {
// CHECK-NOT: init
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> Empty
// CHECK: }
// CHECK: func mutateIt(_: Empty)
// CHECK-NOT: func passThroughByValue(_ x: Empty) -> Empty

// CHECK: class MultipleAttrs {
// CHECK-NOT: init
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> MultipleAttrs
// CHECK: }

// CHECK: class IntPair {
// CHECK-NOT: init
// CHECK:   var a: Int32
// CHECK:   var b: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> IntPair
// CHECK: }
// CHECK: func mutateIt(_ x: IntPair)
// CHECK-NOT: func passThroughByValue(_ x: IntPair) -> IntPair

// CHECK: class RefHoldingPair {
// CHECK-NOT: init
// CHECK-NOT: pair
// CHECK:   var otherValue: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> RefHoldingPair
// CHECK: }

// CHECK: class RefHoldingPairRef {
// CHECK-NOT: init
// CHECK:   var pair: IntPair
// CHECK:   var otherValue: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> RefHoldingPairRef
// CHECK: }

// CHECK: class RefHoldingPairPtr {
// CHECK-NOT: init
// CHECK:   var pair: IntPair
// CHECK:   var otherValue: Int32
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> RefHoldingPairPtr
// CHECK: }

// CHECK: struct ValueHoldingPair {
// CHECK-NOT: init
// CHECK-NOT: pair
// CHECK:   init()
// CHECK:   var otherValue: Int32
// CHECK:   func test() -> Int32
// CHECK:   mutating func testMutable() -> Int32
// CHECK:   static func create() -> UnsafeMutablePointer<ValueHoldingPair>
// CHECK: }

// CHECK: class BigType {
// CHECK-NOT: init
// CHECK:   var a: Int32
// CHECK:   var b: Int32
// CHECK:   var buffer:
// CHECK:   func test() -> Int32
// CHECK:   func testMutable() -> Int32
// CHECK:   class func create() -> BigType
// CHECK: }
// CHECK: func mutateIt(_ x: BigType)
// CHECK-NOT: func passThroughByValue(_ x: BigType) -> BigType
