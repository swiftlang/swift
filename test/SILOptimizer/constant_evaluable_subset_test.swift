// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -primary-file %s -o %t/constant_evaluable_subset_test_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and test the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: not %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 3000 -test-constant-evaluable-subset %t/constant_evaluable_subset_test_silgen.sil > %t/constant_evaluable_subset_test.sil 2> %t/error-output
//
// RUN: %FileCheck %s < %t/error-output
//
// Test the constant evaluator on the output of the mandatory pipeline. This is
// to test that constant evaluability is not affected by mandatory
// optimizations. Note that it can be affected by non-mandatory optimizations,
// especially performance inlining as it inlines functions such as String.+=
// that the evaluator has special knowledge about.
//
// RUN: not %target-sil-opt -silgen-cleanup -diagnose-invalid-escaping-captures -diagnose-static-exclusivity -capture-promotion -access-enforcement-selection -allocbox-to-stack -noreturn-folding -mark-uninitialized-fixup -definite-init -raw-sil-inst-lowering -closure-lifetime-fixup -semantic-arc-opts -destroy-hoisting -ownership-model-eliminator -mandatory-inlining -predictable-memaccess-opts -os-log-optimization -diagnostic-constant-propagation -predictable-deadalloc-elim -guaranteed-arc-opts -diagnose-unreachable -diagnose-infinite-recursion -yield-once-check -dataflow-diagnostics -split-non-cond_br-critical-edges -constexpr-limit 3000 -test-constant-evaluable-subset %t/constant_evaluable_subset_test_silgen.sil > /dev/null 2> %t/error-output-mandatory
//
// RUN: %FileCheck %s < %t/error-output-mandatory

// Test Swift code snippets that are expected to be constant evaluable and those
// that are not. If any of the test here fails, it indicates a change in the
// output of SILGen or the mandatory passes that affects the constant
// evaluability of the corresponding Swift code.

// CHECK-LABEL: @leftShift
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func leftShift(x: Int, y: Int) -> Int {
  return x &<< y
}

// The test driver must only call functions marked as constant evaluable
// with literal arguments.
@_semantics("test_driver")
internal func interpretLeftShiftTest() -> Int {
  return leftShift(x: 10, y: 2)
}

// CHECK-LABEL: @leftShiftWithTraps
// CHECK-NOT: error:
// This is an expensive function to evaluate requiring evaluating approximately
// 1024 instructions.
@_semantics("constant_evaluable")
internal func leftShiftWithTraps(x: Int, y: Int) -> Int {
  return x << y
}

@_semantics("test_driver")
internal func interpretLeftShiftWithTraps() -> Int {
  return leftShiftWithTraps(x: 34, y: 3)
}

// CHECK-LABEL: @rightShift
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func rightShift(x: Int, y: Int) -> Int {
  return x &>> y
}

@_semantics("test_driver")
internal func interpretRightShift() -> Int {
  return rightShift(x: 10, y: 2)
}

// CHECK-LABEL: @rightShiftWithTraps
// CHECK-NOT: error:
// This is an expensive function to evaluate requiring evaluating approximately
// 1024 instructions.
@_semantics("constant_evaluable")
internal func rightShiftWithTraps(x: Int, y: Int) -> Int {
  return x >> y
}

@_semantics("test_driver")
internal func interpretRightShiftWithTraps() -> Int {
  return rightShiftWithTraps(x: 34, y: 3)
}

// CHECK-LABEL: @arithmetic
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func arithmetic(x: Int, y: Int) -> Int {
  let z = x + y
  let w = x &+ z
  let a = w * y
  let b = a &* z
  let c = b - a
  let d = c &- x
  let e = x / d
  return e
}

@_semantics("test_driver")
internal func interpretArithmetic() -> Int {
  return arithmetic(x: 142, y: 212)
}

// CHECK-LABEL: @booleanoperations
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func booleanoperations(a: Bool, b: Bool) -> Bool {
  return (a && b) || (a || b) && !a
}

@_semantics("test_driver")
internal func interpretBooleanOperations() -> Bool {
  return booleanoperations(a: true, b: false)
}

// CHECK-LABEL: @comparisons
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func comparisons(a: Int, b: Int, c: Int8, d: Int8) -> Bool {
  let r1 = a < b
  let r2 = c > d
  return r1 && r2
}

@_semantics("test_driver")
internal func interpretComparisions() -> Bool {
  return comparisons(a: 20, b: 55, c: 56, d: 101)
}

// CHECK-LABEL: @heterogenousIntComparisons
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func heterogenousIntComparisons(a: Int, b: Int16, c: Int8) -> Bool {
  return (a < b) && (c < b)
}

@_semantics("test_driver")
internal func interpretHeterogenousComparisons() -> Bool {
  return heterogenousIntComparisons(a: 101, b: 20, c: 56)
}

// CHECK-LABEL: @bitwiseOperations
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func bitwiseOperations(a: Int16, b: Int16, c: Int16) -> Int16 {
  return a & ((b | c) | ~c)
}

@_semantics("test_driver")
internal func interpretBitWiseOperations() -> Int16 {
  return bitwiseOperations(a: 0xff, b: 0xef, c: 0x7fef)
}

// CHECK-LABEL: @testIntExtensions
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func testIntExtensions(a: Int8, b: Int16) -> Int32 {
  return Int32(a) + Int32(b) + Int32(Int16(a))
}

@_semantics("test_driver")
internal func interpretIntExtensions() -> Int32 {
  return testIntExtensions(a: 100, b: -130)
}

// CHECK-LABEL: @testUIntExtensions
// CHECK-NOT: error:
@_semantics("constant_evaluable")
internal func testUIntExtensions(a: UInt8, b: UInt16) -> UInt32 {
  return UInt32(a) + UInt32(b) + UInt32(UInt16(a))
}

@_semantics("test_driver")
internal func interpretUIntExtensions() -> UInt32 {
  return testUIntExtensions(a: 100, b: 130)
}

// CHECK-LABEL: @testIntTruncations
// CHECK-NOT: error:
// This is an expensive function to evaluate requiring evaluating approximately
// 2048 instructions with optimized stdlib and 3000 instructions with
// unoptimized stdlib.
@_semantics("constant_evaluable")
internal func testIntTruncations(a: Int32) -> Int8 {
  let b = Int16(a)
  let c = Int8(b)
  return c
}

@_semantics("test_driver")
internal func interpretIntTruncations() -> Int8 {
  return testIntTruncations(a: 100)
}

// CHECK-LABEL: @testInvalidIntTruncations
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
internal func testInvalidIntTruncations(a: Int32) -> Int8 {
  return Int8(a)
    // CHECK: note: {{.*}}: Not enough bits to represent the passed value
    // CHECK: note: operation performed during this call traps
    // CHECK: function_ref @$sSZss17FixedWidthIntegerRzrlEyxqd__cSzRd__lufC
}

@_semantics("test_driver")
internal func interpretInvalidIntTruncations() -> Int8 {
  return testInvalidIntTruncations(a: 130)
}

// CHECK-LABEL: @testUIntTruncations
// CHECK-NOT: error:
// This is an expensive function to evaluate requiring evaluating approximately
// 2048 instructions.
@_semantics("constant_evaluable")
internal func testUIntTruncations(a: UInt32) -> UInt8 {
  let b = UInt32(a)
  let c = UInt16(b)
  let d = UInt8(c)
  return d
}

@_semantics("test_driver")
internal func interpretUIntTruncations() -> UInt8 {
  return testUIntTruncations(a: 100)
}

// CHECK-LABEL: @testSingedUnsignedConversions
// CHECK-NOT: error:
// This is an expensive function to evaluate requiring evaluating approximately
// 2048 instructions.
@_semantics("constant_evaluable")
internal func testSingedUnsignedConversions(a: Int32, b: UInt8) -> UInt32 {
  return UInt32(a) + UInt32(Int8(b))
}

@_semantics("test_driver")
internal func interpretSingedUnsignedConversions() -> UInt32 {
  return testSingedUnsignedConversions(a: 100, b: 120)
}

// CHECK-LABEL: @testInvalidSingedUnsignedConversions
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
internal func testInvalidSingedUnsignedConversions(a: Int64) -> UInt64 {
  return UInt64(a)
    // CHECK: note: {{.*}}: Negative value is not representable
    // CHECK: note: operation performed during this call traps
    // CHECK: function_ref @$sSUss17FixedWidthIntegerRzrlEyxqd__cSzRd__lufC
}

@_semantics("test_driver")
internal func interpretInvalidSingedUnsignedConversions() -> UInt64 {
  return testInvalidSingedUnsignedConversions(a: -130)
}

// CHECK-LABEL: @testIO
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
internal func testIO() -> String? {
  return readLine()
    // CHECK: note: encountered call to 'readLine(strippingNewline:)' whose body is not available
    // CHECK: note: function whose body is not available
}

@_semantics("test_driver")
internal func interpretIO() -> String? {
  return testIO()
}

// CHECK-LABEL: @testLoop
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testLoop() -> Int {
  var x = 0
  while x <= 42 {
    x += 1
  }
  return x
    // CHECK: note: control-flow loop found during evaluation
    // CHECK: note: found loop here
}

@_semantics("test_driver")
internal func interpretLoop() -> Int {
  return testLoop()
}

// CHECK-LABEL: @testRecursion
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testRecursion(_ a: Int) -> Int {
  return a <= 0 ? 0 : testRecursion(a-1)
}

@_semantics("test_driver")
internal func interpretRecursion() -> Int {
  return testRecursion(10)
}

// CHECK-LABEL: @testLongRecursion
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testLongRecursion(_ a: Int) -> Int {
  return a == 0 ? 0 : testLongRecursion(a-1)
    // CHECK: note: exceeded instruction limit:
    // CHECK: note: limit exceeded here
}

@_semantics("test_driver")
internal func interpretLongRecursion() -> Int {
  return testLongRecursion(-100)
}

// CHECK-LABEL: @testConditional
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testConditional(_ x: Int) -> Int {
  if x < 0 {
    return 0
  } else {
    return x
  }
}

@_semantics("test_driver")
func interpretConditional() -> Int {
  testConditional(-1)
}

// CHECK-LABEL: @testIntAddOverflow
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testIntAddOverflow(_ x: Int8) -> Int8 {
  return x + 1
    // CHECK: note: integer overflow detected
    // CHECK: note: operation overflows
}

@_semantics("test_driver")
func interpretIntAddOverflow() -> Int8 {
  return testIntAddOverflow(127)
}

// CHECK-LABEL: @testDivideByZero
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testDivideByZero(_ x: Int, _ y: Int) -> Int {
  return x / y
    // CHECK: note: {{.*}}: Division by zero
    // CHECK: note: operation traps
}

@_semantics("test_driver")
func interpretDivideByZero() -> Int {
  return testDivideByZero(127, 0)
}

// CHECK-LABEL: @testDividingFullWidthByZero
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testDividingFullWidthByZero(_ x: Int, _ y: Int, _ z: UInt) -> Int {
  return x.dividingFullWidth((y, z)).1
} // CHECK: note: {{.*}}: Division by zero
  // CHECK: note: operation performed during this call traps

@_semantics("test_driver")
func interpretDividingFullWidthByZero() -> Int {
  return testDividingFullWidthByZero(0, 1, 1)
}

// CHECK-LABEL: @testDivideOverflow
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testDivideOverflow(_ x: Int8, _ y: Int8) -> Int8 {
  return x / y
    // CHECK: note: {{.*}}: Division results in an overflow
    // CHECK: note: operation traps
}

@_semantics("test_driver")
func interpretDivideOverflow() -> Int8 {
  return testDivideOverflow(-128, -1)
}

// CHECK-LABEL: @testDistance
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testDistance(_ x: UInt, _ y: UInt) -> Int {
  return x.distance(to: y)
    // CHECK: note: {{.*}}: Distance is not representable in Int
    // CHECK: note: operation performed during this call traps
}

@_semantics("test_driver")
func interpretDistanceTest() -> Int {
  return testDistance(0, UInt.max)
}

// CHECK-LABEL: @testInOut
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testInOut(_ x: inout Int) {
  x += 1
}

@_semantics("test_driver")
func interpretInOut() -> Int {
  var x = 10
  testInOut(&x)
  return x
}

struct A {
  var x, y: Int

  // CHECK-LABEL: @init(initialValue: Int) -> A
  // CHECK-NOT: error:
  @_semantics("constant_evaluable")
  init(initialValue: Int) {
    x = initialValue
    y = initialValue
  }

  // CHECK-LABEL: @sum
  // CHECK-NOT: error:
  @_semantics("constant_evaluable")
  @_optimize(none)
  func sum() -> Int {
    return x + y
  }

  // CHECK-LABEL: @increment
  // CHECK-NOT: error:
  @_semantics("constant_evaluable")
  @_optimize(none)
  mutating func increment(by step: Int) {
    x += step
    y += step
  }
}

@_semantics("test_driver")
func interpretStructInitAndMethods() -> A {
  var a = A(initialValue: 0)
  let z = a.sum();
  a.increment(by: z)
  return a
}

struct OuterStruct {
  var inner: A
  var z: Int

  // CHECK-LABEL: @sumInner
  // CHECK-NOT: error:
  @_semantics("constant_evaluable")
  @_optimize(none)
  func sumInner() -> Int {
    return inner.sum()
  }
}

@_semantics("test_driver")
func interpretNestedStructAndDefaultInit() -> Int {
  let ostruct = OuterStruct(inner: A(initialValue: 1), z: 10)
  return ostruct.sumInner()
}

struct EmptyStruct {
  func get() -> Int {
    return 0
  }
}

// CHECK-LABEL: @testEmptyStruct
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEmptyStruct() -> Int {
  let emp = EmptyStruct()
  return emp.get()
}

@_semantics("test_driver")
func interpretEmptyStructTest() -> Int {
  return testEmptyStruct()
}

// CHECK-LABEL: @testTuple
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testTuple(_ a: Int, _ b: Bool) -> Bool {
  func extractSecond(_ tuple: (Int, Bool)) -> Bool {
    return tuple.1
  }
  return extractSecond((a, b))
}

@_semantics("test_driver")
func interpretTuple() -> Bool {
  return testTuple(10, false)
}

// CHECK-LABEL: @testGenericFunction
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testGenericFunction<T: Equatable>(_ a: T, _ b: T) -> Bool {
  return a == b
}

@_semantics("test_driver")
func interpretGenericFunction() -> Bool {
  return testGenericFunction(10, 11)
}

protocol P {
  mutating func clear() -> Int
}

struct PImpl: P {
  var i = 100
  mutating func clear() -> Int {
    let prev = i
    i = 0
    return prev
  }
}

// CHECK-LABEL: @testCustomGenericConstraint
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testCustomGenericConstraint<T: P>(_ a: inout T) -> Int {
  return a.clear()
}

@_semantics("test_driver")
func interpretCustomGenericConstraint() -> Int {
  var s = PImpl();
  return testCustomGenericConstraint(&s)
}

// CHECK-LABEL: @testProtocolMethodDispatch
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testProtocolMethodDispatch(_ s: PImpl) -> Int {
  func innerFunc(_ proto: P) -> Int {
    var tmp = proto
    return tmp.clear()
  }
  return innerFunc(s)
    // CHECK: note: encountered operation not supported by the evaluator: init_existential_addr
    // CHECK: note: operation not supported by the evaluator
}

@_semantics("test_driver")
func interpretProtocolMethodDispatch() -> Int {
  return testProtocolMethodDispatch(PImpl())
}

struct SGeneric<X, Y> {
  var x: X
  var y: Y

  // CHECK-LABEL: @methodWithGeneric
  // CHECK-NOT: error:
  @_semantics("constant_evaluable")
  @_optimize(none)
  func methodWithGeneric<Z>(_ z: Z) -> SGeneric<Z, Y> {
    return SGeneric<Z, Y>(x: z, y: y)
  }
}

@_semantics("test_driver")
func interpretStructAndMethodWithGeneric() -> SGeneric<Int, Bool> {
  let s = SGeneric<Int8, Bool>(x: 10, y: true)
  return s.methodWithGeneric(240)
}

protocol ProtoWithInit {
  init(_ x: Int)
}

struct C : ProtoWithInit {
  init(_ x: Int) {
  }
}

// CHECK-LABEL: @testGenericConstruction
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testGenericConstruction<T: ProtoWithInit>(_: T.Type) -> T {
  return T(0)
}

@_semantics("test_driver")
func interpretGenericConstruction() -> C {
  return testGenericConstruction(C.self)
}

// CHECK-LABEL: @testSupportedStringOperations
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testSupportedStringOperations(_ x: String, _ y: String) -> Bool {
  var z = x
  z += y
  return z == x
}

@_semantics("test_driver")
func interpretSupportedStringOperations() -> Bool {
  return testSupportedStringOperations("abc", "zyx")
}

// CHECK-LABEL: @testOptional
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testOptional(_ xopt: String?) -> String {
  if let x = xopt {
    return x
  }
  return ""
}

@_semantics("test_driver")
func interpretOptionalTest() -> String {
  return testOptional("a")
}

enum Side {
  case right
  case left
}

// CHECK-LABEL: @testEnumSwitch
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEnumSwitch(_ side: Side) -> Int {
  switch side {
  case .right:
    return 1
  case .left:
    return 0
  }
}

@_semantics("test_driver")
func interpretEnumSwitch() -> Int {
  return testEnumSwitch(.right)
}

// CHECK-LABEL: @testEnumEquality
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEnumEquality(_ side: Side, _ otherSide: Side) -> Bool {
  return side == otherSide
}

@_semantics("test_driver")
func interpretEnumEquality() -> Bool {
  return testEnumEquality(.right, .left)
}

enum Shape {
  case circle(radius: Int)
  case rectangle(length: Int, breadth: Int)
}

// CHECK-LABEL: @testEnumWithData
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEnumWithData(_ shape: Shape) -> Int {
  switch shape {
  case .circle(radius: let x):
    return x
  case .rectangle(length: let x, breadth: let y):
    return x + y
  }
}

@_semantics("test_driver")
func interpretEnumWithData() -> Int {
  return testEnumWithData(.circle(radius: 11))
}

enum Number<T: BinaryInteger> {
  case integer(T)
  case rational(T, T)
}

// CHECK-LABEL: @testAddressOnlyEnum
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testAddressOnlyEnum<T: BinaryInteger>(_ number: Number<T>) -> T {
  switch number {
  case .integer(let x):
    return x
  case .rational(let x, let y):
    return x + y
  }
}

@_semantics("test_driver")
func interpretAddressOnlyEnum() -> Int {
  return testAddressOnlyEnum(.rational(22, 7))
}

indirect enum Nat {
  case zero
  case succ(Nat)
}

// CHECK-LABEL: @testIndirectEnum
// CHECK: error: not constant evaluable
@_semantics("constant_evaluable")
func testIndirectEnum(_ nat: Nat) -> Bool {
  switch nat {
  case .zero:
    return true
  case .succ:
    return false
  }
    // CHECK-NOTE:  note: encountered operation not supported by the evaluator: alloc_box
}

@_semantics("test_driver")
func interpretIndirectEnum() -> Bool {
  return testIndirectEnum(.succ(.zero))
}

// CHECK-LABEL: @testEmptyArrayInit
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEmptyArrayInit() -> [Int] {
  return Array<Int>()
}

@_semantics("test_driver")
func interpretEmptyArrayInit() -> [Int] {
  return testEmptyArrayInit()
}

// CHECK-LABEL: @testEmptyArrayLiteral
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testEmptyArrayLiteral() -> [Int] {
  return []
}

@_semantics("test_driver")
func interpretEmptyArrayLiteral() -> [Int] {
  return testEmptyArrayLiteral()
}

// CHECK-LABEL: @testArrayLiteral
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testArrayLiteral(_ x: Int, _ y: Int) -> [Int] {
  return [x, y, 4]
}

@_semantics("test_driver")
func interpretArrayLiteral() -> [Int] {
  return testArrayLiteral(2, 3)
}

// CHECK-LABEL: @testArrayAppend
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testArrayAppend(_ x: Int) -> [Int] {
  var a: [Int] = []
  a.append(x)
  return a
}

@_semantics("test_driver")
func interpretArrayAppend() -> [Int] {
  return testArrayAppend(25)
}

// CHECK-LABEL: @testArrayAppendNonEmpty
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testArrayAppendNonEmpty(_ x: String) -> [String] {
  var a: [String] = ["ls", "cat", "echo", "cd"]
  a.append(x)
  return a
}

@_semantics("test_driver")
func interpretArrayAppendNonEmpty() -> [String] {
  return testArrayAppendNonEmpty("mkdir")
}

struct StructContaningArray {
  var array: [Int]
}

// CHECK-LABEL: @testArrayFieldAppend
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testArrayFieldAppend(_ x: Int) -> StructContaningArray {
  var s = StructContaningArray(array: [])
  s.array.append(x)
  return s
}

@_semantics("test_driver")
func interpretArrayFieldAppend() -> StructContaningArray {
  return testArrayFieldAppend(0)
}

// CHECK-LABEL: @testClosureInit
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testClosureInit(_ x: Int) -> () -> Int {
  return { x }
}

@_semantics("test_driver")
func interpretClosureCreation() -> () -> Int {
  return testClosureInit(19)
}

// CHECK-LABEL: @testClosureChaining
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testClosureChaining(_ x: Int, _ y: Int) -> () -> Int {
  let clo: (Int) -> Int = { $0 + x }
  return { clo(y) }
}

@_semantics("test_driver")
func interpretClosureChains() -> () -> Int {
  return testClosureChaining(191, 201)
}

// CHECK-LABEL: @testClosureWithNonConstantCaptures
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testClosureWithNonConstantCaptures(_ x: @escaping () -> Int) -> () -> Int {
  return x
}

@_semantics("test_driver")
func interpretClosureWithNonConstantCaptures(_ x: Int) -> () -> Int {
  return testClosureWithNonConstantCaptures({ x })
}

// CHECK-LABEL: @testAutoClosure
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testAutoClosure(_ x: @escaping @autoclosure () -> Int) -> () -> Int {
  return x
}

@_semantics("test_driver")
func interpretAutoClosure(_ x: Int) -> () -> Int {
  return testAutoClosure(x)
}

// Test thin-to-thick function conversion.

func someFunction(_ x: Int) -> Int {
  return x + 1
}

// CHECK-LABEL: @testThinToThick
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testThinToThick() -> (Int) -> Int {
  return someFunction
}

@_semantics("test_driver")
func interpretThinToThick() -> (Int) -> Int {
  return testThinToThick()
}

// Test closures and arrays combination.

// CHECK-LABEL: @testArrayOfClosures
// CHECK-NOT: error:
@_semantics("constant_evaluable")
func testArrayOfClosures(_ byte: @escaping () -> Int) -> [(Int) -> Int] {
  var closureArray: [(Int) -> Int] = []
  // Append a simple closure.
  closureArray.append({ arg in
    return 0
  })
  // Append a closure that does computation.
  closureArray.append({ arg in
    return byte() + arg
  })
  return closureArray
}

@_semantics("test_driver")
func interpretArrayOfClosures() -> [(Int) -> Int] {
  return testArrayOfClosures({ 10 })
}
