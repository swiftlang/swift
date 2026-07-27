// RUN: %target-swift-ide-test -print-module -module-to-print=POD -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: class Empty {
// CHECK: init
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> Empty
// CHECK: }
// CHECK: func takesConstRefEmpty(_ e: Empty)
// CHECK: func takesConstRefEmptyDefaulted(_ e: Empty)
// CHECK: func mutateIt(_: Empty)
// CHECK-NOT: func passThroughByValue(_ x: Empty) -> Empty

// CHECK: class MultipleAttrs {
// CHECK: init
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> MultipleAttrs
// CHECK: }

// CHECK: class IntPair {
// CHECK: init
// CHECK:   var a: CInt
// CHECK:   var b: CInt
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   func instancePassThroughByRef(_ ref: IntPair) -> IntPair
// CHECK:   class func staticPassThroughByRef(_ ref: IntPair) -> IntPair
// CHECK:   class func create() -> IntPair
// CHECK: }
// CHECK: func mutateIt(_ x: IntPair)
// CHECK-NOT: func passThroughByValue(_ x: IntPair) -> IntPair
// CHECK: func passThroughByRef(_ x: IntPair) -> IntPair

// CHECK: class RefHoldingPair {
// CHECK: init
// CHECK-NOT: pair
// CHECK:   var otherValue: CInt
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> RefHoldingPair
// CHECK: }

// CHECK: class RefHoldingPairRef {
// CHECK: init
// CHECK:   var pair: IntPair
// CHECK:   var otherValue: CInt
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> RefHoldingPairRef
// CHECK: }

// CHECK: class RefHoldingPairPtr {
// CHECK: init
// CHECK:   var pair: IntPair
// CHECK:   var otherValue: CInt
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> RefHoldingPairPtr
// CHECK: }

// CHECK: struct ValueHoldingPair {
// CHECK-NOT: pair
// CHECK:   init()
// CHECK:   var otherValue: CInt
// CHECK:   func test() -> CInt
// CHECK:   mutating func testMutable() -> CInt
// CHECK:   static func create() -> UnsafeMutablePointer<ValueHoldingPair>
// CHECK: }

// CHECK: struct ValueHoldingPairRef {
// CHECK-NEXT:   init(pair: IntPair)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var pair: IntPair
// CHECK-NEXT:   func sub(_ other: IntPair) -> CInt
// CHECK-NEXT:   func max(_ other: IntPair) -> IntPair
// CHECK-NEXT: }

// CHECK: class BigType {
// CHECK: init
// CHECK:   var a: CInt
// CHECK:   var b: CInt
// CHECK:   var buffer:
// CHECK:   func test() -> CInt
// CHECK:   func testMutable() -> CInt
// CHECK:   class func create() -> BigType
// CHECK: }
// CHECK: func mutateIt(_ x: BigType)
// CHECK-NOT: func passThroughByValue(_ x: BigType) -> BigType
