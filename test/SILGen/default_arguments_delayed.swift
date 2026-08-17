// RUN: %target-swift-emit-silgen %s
// TODO: filecheck

protocol P {}
protocol Q {}

protocol HasDefault {
  static var defaultValue: Self { get }
}

// MARK: - Core Bug: Default Arguments Before inout Generic Parameters

// CHECK-LABEL: sil hidden @$s{{.*}}24testDefaultBeforeInout
func testDefaultBeforeInout(_ input: any P) {
  var local = input
  fnDefaultBeforeInout(b: &local)
}
// CHECK: [[OPEN:%.*]] = open_existential_addr
// CHECK: [[DEFAULT_FN:%.*]] = function_ref @$s{{.*}}fnDefaultBeforeInout
// CHECK: apply [[DEFAULT_FN]]
// Verify open_existential_addr dominates the default argument initializer

func fnDefaultBeforeInout<Value: P>(a: Bool = false, b: inout Value) {}

// CHECK-LABEL: sil hidden @$s{{.*}}31testMultipleDefaultsBeforeInout
func testMultipleDefaultsBeforeInout(_ input: any P) {
  var local = input
  fnMultipleDefaults(c: &local)
}

func fnMultipleDefaults<Value: P>(a: Int = 1, b: String = "test", c: inout Value) {}

// MARK: - Control: Default Arguments After inout (Should Work)

// CHECK-LABEL: sil hidden @$s{{.*}}23testDefaultAfterInout
func testDefaultAfterInout(_ input: any P) {
  var local = input
  fnDefaultAfterInout(a: &local)
}

func fnDefaultAfterInout<Value: P>(a: inout Value, b: Bool = false) {}

// MARK: - Parameter Type Variations

// CHECK-LABEL: sil hidden @$s{{.*}}30testDefaultBeforeNonInoutGeneric
func testDefaultBeforeNonInoutGeneric(_ input: any P) {
  fnNonInout(b: input)
}

func fnNonInout<Value: P>(a: Bool = false, b: Value) {}

// CHECK-LABEL: sil hidden @$s{{.*}}32testDefaultBeforeBorrowingGeneric
func testDefaultBeforeBorrowingGeneric(_ input: any P) {
  fnBorrowing(b: input)
}

func fnBorrowing<Value: P>(a: Bool = false, b: borrowing Value) {}

// CHECK-LABEL: sil hidden @$s{{.*}}32testDefaultBeforeConsumingGeneric
func testDefaultBeforeConsumingGeneric(_ input: any P) {
  fnConsuming(b: input)
}

func fnConsuming<Value: P>(a: Bool = false, b: consuming Value) {}

// MARK: - Multiple Generic Parameters

// CHECK-LABEL: sil hidden @$s{{.*}}28testTwoGenericsDefaultBefore
func testTwoGenericsDefaultBefore(_ p: any P, _ q: any Q) {
  var localP = p
  var localQ = q
  fnTwoGenerics(b: &localP, c: &localQ)
}

func fnTwoGenerics<T: P, U: Q>(a: Bool = false, b: inout T, c: inout U) {}

// CHECK-LABEL: sil hidden @$s{{.*}}29testDefaultBetweenTwoGenerics
func testDefaultBetweenTwoGenerics(_ p: any P, _ q: any P) {
  var localP = p
  var localQ = q
  fnDefaultBetween(a: &localP, c: &localQ)
}

func fnDefaultBetween<T: P, U: P>(a: inout T, b: Bool = false, c: inout U) {}

// MARK: - TextOutputStream

protocol CustomOutputStream: TextOutputStream {}

// CHECK-LABEL: sil hidden @$s{{.*}}28testCustomOutputStreamPrint
func testCustomOutputStreamPrint(_ stream: any CustomOutputStream) {
  var local = stream
  customPrint("test", to: &local)
}

func customPrint<Target: CustomOutputStream>(
  _ items: String...,
  separator: String = " ",
  terminator: String = "\n",
  to output: inout Target
) {}

// MARK: - Complex Default Argument Expressions

// Default argument using generic type parameter - should fail or be diagnosed
func fnWithGenericDefault<Value: HasDefault>(
  a: Value = Value.defaultValue,
  b: inout Value
) {}

// CHECK-LABEL: sil hidden @$s{{.*}}31testDefaultWithClosureCapture
func testDefaultWithClosureCapture(_ input: any P) {
  var local = input
  fnClosureDefault(b: &local)
}

func fnClosureDefault<Value: P>(
  a: () -> Int = { 42 },
  b: inout Value
) {}

// MARK: - Access Patterns

struct Container {
  var value: any P
  
  mutating func process<T: P>(flag: Bool = true, item: inout T) {}
  
  // CHECK-LABEL: sil hidden @$s{{.*}}9Container16testNestedAccess
  mutating func testNestedAccess() {
    var local = value
    process(item: &local)
  }
}

// CHECK-LABEL: sil hidden @$s{{.*}}26testMultipleSuccessiveCalls
func testMultipleSuccessiveCalls(_ input: any P) {
  var local = input
  fnDefaultBeforeInout(b: &local)
  fnDefaultBeforeInout(b: &local)
}

// MARK: - Optional Parameters (SE-0375)

// CHECK-LABEL: sil hidden @$s{{.*}}32testDefaultBeforeOptionalGeneric
func testDefaultBeforeOptionalGeneric(_ input: any P) {
  fnOptional(b: input)
}

func fnOptional<Value: P>(a: Bool = false, b: Value?) {}

// CHECK-LABEL: sil hidden @$s{{.*}}37testDefaultBeforeOptionalInoutGeneric

// TODO: figure out what's up with this one....
// func testDefaultBeforeOptionalInoutGeneric(_ input: any P) {
//   var local: (any P)? = input
//   fnOptionalInout(b: &local)
// }

func fnOptionalInout<Value: P>(a: Bool = false, b: inout Value?) {}

// MARK: - Workarounds

// Explicit opening via helper function
// CHECK-LABEL: sil hidden @$s{{.*}}23testExplicitOpenHelper
func testExplicitOpenHelper(_ input: any P) {
  func helper<T: P>(_ value: inout T) {
    fnDefaultBeforeInout(b: &value)
  }
  var local = input
  helper(&local)
}

// Providing explicit value for default parameter
// CHECK-LABEL: sil hidden @$s{{.*}}25testExplicitDefaultValue
func testExplicitDefaultValue(_ input: any P) {
  var local = input
  fnDefaultBeforeInout(a: false, b: &local)
}

// Reordered parameters (default after generic)
// CHECK-LABEL: sil hidden @$s{{.*}}22testReorderedParameters
func testReorderedParameters(_ input: any P) {
  var local = input
  fnReordered(a: &local)
}

func fnReordered<Value: P>(a: inout Value, b: Bool = false) {}

// MARK: - Edge Cases

// CHECK-LABEL: sil hidden @$s{{.*}}33testVariadicWithDefaultAndGeneric
func testVariadicWithDefaultAndGeneric(_ input: any P) {
  var local = input
  fnVariadic(1, 2, 3, value: &local)
}

func fnVariadic<Value: P>(
  _ items: Int...,
  flag: Bool = false,
  value: inout Value
) {}

// CHECK-LABEL: sil hidden @$s{{.*}}28testAutoclosureDefaultArg
func testAutoclosureDefaultArg(_ input: any P) {
  var local = input
  fnAutoclosure(value: &local)
}

func fnAutoclosure<Value: P>(
  condition: @autoclosure () -> Bool = true,
  value: inout Value
) {}

// MARK: - Mixed Scenarios

// CHECK-LABEL: sil hidden @$s{{.*}}40testDefaultBeforeInoutAndRegularGeneric
func testDefaultBeforeInoutAndRegularGeneric(_ p1: any P, _ p2: any P) {
  var local = p1
  fnMixedParams(b: &local, c: p2)
}

func fnMixedParams<T: P, U: P>(
  a: Bool = false,
  b: inout T,
  c: U,
  d: String = "default"
) {}

// CHECK-LABEL: sil hidden @$s{{.*}}41testMultipleExistentialsMultipleDefaults
func testMultipleExistentialsMultipleDefaults(_ p: any P, _ q: any Q) {
  var localP = p
  var localQ = q
  fnMultipleExistentials(b: &localP, d: &localQ)
}

func fnMultipleExistentials<T: P, U: Q>(
  a: Int = 0,
  b: inout T,
  c: String = "test",
  d: inout U,
  e: Bool = true
) {}

// MARK: - Contextual Generic Functions

class GenericContext<T: P> {
  // CHECK-LABEL: sil hidden @$s{{.*}}14GenericContext26testDefaultInGenericContext
  func testDefaultInGenericContext(_ input: any P) {
    var local = input
    process(item: &local)
  }
  
  func process<U: P>(flag: Bool = true, item: inout U) {}
}

// MARK: - Protocol Extensions

extension P {
  // CHECK-LABEL: sil hidden @$s{{.*}}1P31testDefaultInProtocolExtension
  mutating func testDefaultInProtocolExtension<U: P>(a: Bool = true, _ other: inout U, flag: Bool = false) {}
  
  // CHECK-LABEL: sil hidden @$s{{.*}}1P20callFromExtension
  mutating func callFromExtension(_ input: any P) {
    var local = input
    testDefaultInProtocolExtension(&local)
  }
}
