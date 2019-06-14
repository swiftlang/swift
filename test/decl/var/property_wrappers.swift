// RUN: %target-typecheck-verify-swift -swift-version 5

// ---------------------------------------------------------------------------
// Property wrapper type definitions
// ---------------------------------------------------------------------------
@propertyWrapper
struct Wrapper<T> {
  var value: T
}

@propertyWrapper
struct WrapperWithInitialValue<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

@propertyWrapper
struct WrapperAcceptingAutoclosure<T> {
  private let fn: () -> T

  var value: T {
    return fn()
  }

  init(initialValue fn: @autoclosure @escaping () -> T) {
    self.fn = fn
  }

  init(body fn: @escaping () -> T) {
    self.fn = fn
  }
}

@propertyWrapper
struct MissingValue<T> { }
// expected-error@-1{{property wrapper type 'MissingValue' does not contain a non-static property named 'value'}}

@propertyWrapper
struct StaticValue {
  static var value: Int = 17
}
// expected-error@-3{{property wrapper type 'StaticValue' does not contain a non-static property named 'value'}}


// expected-error@+1{{'@propertyWrapper' attribute cannot be applied to this declaration}}
@propertyWrapper
protocol CannotBeAWrapper {
  associatedtype Value
  var value: Value { get set }
}

@propertyWrapper
struct NonVisibleValueWrapper<Value> {
  private var value: Value // expected-error{{private property 'value' cannot have more restrictive access than its enclosing property wrapper type 'NonVisibleValueWrapper' (which is internal)}}
}

@propertyWrapper
struct NonVisibleInitWrapper<Value> {
  var value: Value

  private init(initialValue: Value) { // expected-error{{private initializer 'init(initialValue:)' cannot have more restrictive access than its enclosing property wrapper type 'NonVisibleInitWrapper' (which is internal)}}
    self.value = initialValue
  }
}

@propertyWrapper
struct InitialValueTypeMismatch<Value> {
  var value: Value // expected-note{{'value' declared here}}

  init(initialValue: Value?) { // expected-error{{'init(initialValue:)' parameter type ('Value?') must be the same as its 'value' property type ('Value') or an @autoclosure thereof}}
    self.value = initialValue!
  }
}

@propertyWrapper
struct MultipleInitialValues<Value> { // expected-error{{property wrapper type 'MultipleInitialValues' has multiple initial-value initializers}}
  var value: Value? = nil

  init(initialValue: Int) { // expected-note{{initializer 'init(initialValue:)' declared here}}
  }

  init(initialValue: Double) { // expected-note{{initializer 'init(initialValue:)' declared here}}
  }
}

@propertyWrapper
struct InitialValueFailable<Value> {
  var value: Value

  init?(initialValue: Value) { // expected-error{{'init(initialValue:)' cannot be failable}}
    return nil
  }
}

@propertyWrapper
struct InitialValueFailableIUO<Value> {
  var value: Value

  init!(initialValue: Value) {  // expected-error{{'init(initialValue:)' cannot be failable}}
    return nil
  }
}

// ---------------------------------------------------------------------------
// Property wrapper type definitions
// ---------------------------------------------------------------------------
@propertyWrapper
struct _lowercaseWrapper<T> {
  var value: T
}

@propertyWrapper
struct _UppercaseWrapper<T> {
  var value: T
}

// ---------------------------------------------------------------------------
// Limitations on where property wrappers can be used
// ---------------------------------------------------------------------------

func testLocalContext() {
  @WrapperWithInitialValue // expected-error{{property wrappers are not yet supported on local properties}}
  var x = 17
  x = 42
  _ = x
}

enum SomeEnum {
  case foo

  @Wrapper(value: 17)
  var bar: Int // expected-error{{property 'bar' declared inside an enum cannot have a wrapper}}
  // expected-error@-1{{enums must not contain stored properties}}

  @Wrapper(value: 17)
  static var x: Int
}

protocol SomeProtocol {
  @Wrapper(value: 17)
  var bar: Int // expected-error{{property 'bar' declared inside a protocol cannot have a wrapper}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}

  @Wrapper(value: 17)
  static var x: Int // expected-error{{property 'x' declared inside a protocol cannot have a wrapper}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}
}

struct HasWrapper { }

extension HasWrapper {
  @Wrapper(value: 17)
  var inExt: Int // expected-error{{property 'inExt' declared inside an extension cannot have a wrapper}}
  // expected-error@-1{{extensions must not contain stored properties}}

  @Wrapper(value: 17)
  static var x: Int
}

class ClassWithWrappers {
  @Wrapper(value: 17)
  var x: Int
}

class Superclass {
  var x: Int = 0
}

class SubclassOfClassWithWrappers: ClassWithWrappers {
  override var x: Int {
    get { return super.x }
    set { super.x = newValue }
  }
}

class SubclassWithWrapper: Superclass {
  @Wrapper(value: 17)
  override var x: Int { get { return 0 } set { } } // expected-error{{property 'x' with attached wrapper cannot override another property}}
}

class C { }

struct BadCombinations {
  @WrapperWithInitialValue
  lazy var x: C = C() // expected-error{{property 'x' with a wrapper cannot also be lazy}}
  @Wrapper
  weak var y: C? // expected-error{{property 'y' with a wrapper cannot also be weak}}
  @Wrapper
  unowned var z: C // expected-error{{property 'z' with a wrapper cannot also be unowned}}
}

struct MultipleWrappers {
  @Wrapper(value: 17) // expected-error{{cannot convert value of type 'Wrapper<Int>' to specified type 'Int'}}
  @WrapperWithInitialValue
  var x: Int = 17 // expected-error{{property 'x' with attached wrapper cannot initialize both the wrapper type and the property}}

  @WrapperWithInitialValue // expected-error 2{{property wrapper can only apply to a single variable}}
  var (y, z) = (1, 2)
}

// ---------------------------------------------------------------------------
// Initialization
// ---------------------------------------------------------------------------

struct Initialization {
  @Wrapper(value: 17)
  var x: Int

  @Wrapper(value: 17)
  var x2: Double

  @Wrapper(value: 17)
  var x3 = 42 // expected-error{{property 'x3' with attached wrapper cannot initialize both the wrapper type and the property}}

  @Wrapper(value: 17)
  var x4

  @WrapperWithInitialValue
  var y = true

  // FIXME: It would be nice if we had a more detailed diagnostic here.
  @WrapperWithInitialValue<Int>
  var y2 = true // expected-error{{'Bool' is not convertible to 'Int'}}

  mutating func checkTypes(s: String) {
    x2 = s // expected-error{{cannot assign value of type 'String' to type 'Double'}}
    x4 = s // expected-error{{cannot assign value of type 'String' to type 'Int'}}
    y = s // expected-error{{cannot assign value of type 'String' to type 'Bool'}}
  }
}

// ---------------------------------------------------------------------------
// Wrapper type formation
// ---------------------------------------------------------------------------
@propertyWrapper
struct IntWrapper {
  var value: Int
}

@propertyWrapper
struct WrapperForHashable<T: Hashable> { // expected-note{{property wrapper type 'WrapperForHashable' declared here}}
  var value: T
}

@propertyWrapper
struct WrapperWithTwoParams<T, U> {
  var value: (T, U)
}

struct NotHashable { }

struct UseWrappersWithDifferentForm {
  @IntWrapper
  var x: Int

  // FIXME: Diagnostic should be better here
  @WrapperForHashable
  var y: NotHashable // expected-error{{property type 'NotHashable' does not match that of the 'value' property of its wrapper type 'WrapperForHashable'}}

  @WrapperForHashable
  var yOkay: Int

  @WrapperWithTwoParams
  var zOkay: (Int, Float)

  // FIXME: Need a better diagnostic here
  @HasNestedWrapper.NestedWrapper
  var w: Int // expected-error{{property type 'Int' does not match that of the 'value' property of its wrapper type 'HasNestedWrapper.NestedWrapper'}}

  @HasNestedWrapper<Double>.NestedWrapper
  var wOkay: Int

  @HasNestedWrapper.ConcreteNestedWrapper
  var wOkay2: Int
}

@propertyWrapper
struct Function<T, U> { // expected-note{{property wrapper type 'Function' declared here}}
  var value: (T) -> U?
}

struct TestFunction {
  @Function var f: (Int) -> Float?
  
  @Function var f2: (Int) -> Float // expected-error{{property type '(Int) -> Float' does not match that of the 'value' property of its wrapper type 'Function'}}

  func test() {
    let _: Int = $f // expected-error{{cannot convert value of type 'Function<Int, Float>' to specified type 'Int'}}
  }
}

// ---------------------------------------------------------------------------
// Nested wrappers
// ---------------------------------------------------------------------------
struct HasNestedWrapper<T> {
  @propertyWrapper
  struct NestedWrapper<U> { // expected-note{{property wrapper type 'NestedWrapper' declared here}}
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  @propertyWrapper
  struct ConcreteNestedWrapper {
    var value: T
    init(initialValue: T) {
      self.value = initialValue
    }
  }

  @NestedWrapper
  var y: [T] = []
}

struct UsesNestedWrapper<V> {
  @HasNestedWrapper<V>.NestedWrapper
  var y: [V]
}

// ---------------------------------------------------------------------------
// Referencing the backing store
// ---------------------------------------------------------------------------
struct BackingStore<T> {
  @Wrapper
  var x: T

  @WrapperWithInitialValue
  private var y = true  // expected-note{{'y' declared here}}

  func getXStorage() -> Wrapper<T> {
    return $x
  }

  func getYStorage() -> WrapperWithInitialValue<Bool> {
    return self.$y
  }
}

func testBackingStore<T>(bs: BackingStore<T>) {
  _ = bs.x
  _ = bs.y // expected-error{{'y' is inaccessible due to 'private' protection level}}
}

// ---------------------------------------------------------------------------
// Explicitly-specified accessors
// ---------------------------------------------------------------------------
struct WrapperWithAccessors {
  @Wrapper  // expected-error{{property wrapper cannot be applied to a computed property}}
  var x: Int {
    return 17
  }
}

struct UseWillSetDidSet {
  @Wrapper
  var x: Int {
    willSet {
      print(newValue)
    }
  }

  @Wrapper
  var y: Int {
    didSet {
      print(oldValue)
    }
  }

  @Wrapper
  var z: Int {
    willSet {
      print(newValue)
    }

    didSet {
      print(oldValue)
    }
  }
}

// ---------------------------------------------------------------------------
// Mutating/nonmutating
// ---------------------------------------------------------------------------
@propertyWrapper
struct WrapperWithNonMutatingSetter<Value> {
  class Box {
    var value: Value
    init(value: Value) {
      self.value = value
    }
  }

  var box: Box

  init(initialValue: Value) {
    self.box = Box(value: initialValue)
  }

  var value: Value {
    get { return box.value }
    nonmutating set { box.value = newValue }
  }
}

@propertyWrapper
struct WrapperWithMutatingGetter<Value> {
  var readCount = 0
  var writeCount = 0
  var stored: Value

  init(initialValue: Value) {
    self.stored = initialValue
  }

  var value: Value {
    mutating get {
      readCount += 1
      return stored
    }
    set {
      writeCount += 1
      stored = newValue
    }
  }
}

@propertyWrapper
class ClassWrapper<Value> {
  var value: Value

  init(initialValue: Value) {
    self.value = initialValue
  }
}

struct UseMutatingnessWrappers {
  @WrapperWithNonMutatingSetter
  var x = true

  @WrapperWithMutatingGetter
  var y = 17

  @WrapperWithNonMutatingSetter // expected-error{{property wrapper can only be applied to a 'var'}}
  let z = 3.14159 // expected-note 2{{change 'let' to 'var' to make it mutable}}

  @ClassWrapper
  var w = "Hello"
}

func testMutatingness() {
  var mutable = UseMutatingnessWrappers()

  _ = mutable.x
  mutable.x = false

  _ = mutable.y
  mutable.y = 42

  _ = mutable.z
  mutable.z = 2.71828 // expected-error{{cannot assign to property: 'z' is a 'let' constant}}

  _ = mutable.w
  mutable.w = "Goodbye"

  let nonmutable = UseMutatingnessWrappers() // expected-note 2{{change 'let' to 'var' to make it mutable}}

  // Okay due to nonmutating setter
  _ = nonmutable.x
  nonmutable.x = false

  _ = nonmutable.y // expected-error{{cannot use mutating getter on immutable value: 'nonmutable' is a 'let' constant}}
  nonmutable.y = 42 // expected-error{{cannot use mutating getter on immutable value: 'nonmutable' is a 'let' constant}}

  _ = nonmutable.z
  nonmutable.z = 2.71828 // expected-error{{cannot assign to property: 'z' is a 'let' constant}}

  // Okay due to implicitly nonmutating setter
  _ = nonmutable.w
  nonmutable.w = "World"
}

// ---------------------------------------------------------------------------
// Access control
// ---------------------------------------------------------------------------
struct HasPrivateWrapper<T> {
  @propertyWrapper
  private struct PrivateWrapper<U> { // expected-note{{type declared here}}
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  @PrivateWrapper
  var y: [T] = []
  // expected-error@-1{{property must be declared private because its property wrapper type uses a private type}}

  // Okay to reference private entities from a private property
  @PrivateWrapper
  private var z: [T]
}

public struct HasUsableFromInlineWrapper<T> {
  @propertyWrapper
  struct InternalWrapper<U> { // expected-note{{type declared here}}
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  @InternalWrapper
  @usableFromInline
  var y: [T] = []
  // expected-error@-1{{property wrapper type referenced from a '@usableFromInline' property must be '@usableFromInline' or public}}
}

@propertyWrapper
class Box<Value> {
  private(set) var value: Value

  init(initialValue: Value) {
    self.value = initialValue
  }
}

struct UseBox {
  @Box
  var x = 17 // expected-note{{'$x' declared here}}
}

func testBox(ub: UseBox) {
  _ = ub.x
  ub.x = 5 // expected-error{{cannot assign to property: 'x' is a get-only property}}

  var mutableUB = ub
  mutableUB = ub
}

func backingVarIsPrivate(ub: UseBox) {
  _ = ub.$x // expected-error{{'$x' is inaccessible due to 'private' protection level}}
}

// ---------------------------------------------------------------------------
// Memberwise initializers
// ---------------------------------------------------------------------------
struct MemberwiseInits<T> {
  @Wrapper
  var x: Bool

  @WrapperWithInitialValue
  var y: T
}

func testMemberwiseInits() {
  // expected-error@+1{{type '(Wrapper<Bool>, Double) -> MemberwiseInits<Double>'}}
  let _: Int = MemberwiseInits<Double>.init

  _ = MemberwiseInits(x: Wrapper(value: true), y: 17)
}

struct DefaultedMemberwiseInits {
  @Wrapper(value: true)
  var x: Bool

  @WrapperWithInitialValue
  var y: Int = 17

  @WrapperWithInitialValue(initialValue: 17)
  var z: Int
}

func testDefaultedMemberwiseInits() {
  _ = DefaultedMemberwiseInits()
  _ = DefaultedMemberwiseInits(
    x: Wrapper(value: false),
    y: 42,
    z: WrapperWithInitialValue(initialValue: 42))

  _ = DefaultedMemberwiseInits(y: 42)
  _ = DefaultedMemberwiseInits(x: Wrapper(value: false))
  _ = DefaultedMemberwiseInits(z: WrapperWithInitialValue(initialValue: 42))
}

// ---------------------------------------------------------------------------
// Default initializers
// ---------------------------------------------------------------------------
struct DefaultInitializerStruct {
  @Wrapper(value: true)
  var x

  @WrapperWithInitialValue
  var y: Int = 10
}

struct NoDefaultInitializerStruct { // expected-note{{'init(x:)' declared here}}
  @Wrapper
  var x: Bool
}

class DefaultInitializerClass {
  @Wrapper(value: true)
  var x

  @WrapperWithInitialValue
  final var y: Int = 10
}

class NoDefaultInitializerClass { // expected-error{{class 'NoDefaultInitializerClass' has no initializers}}
  @Wrapper
  final var x: Bool  // expected-note{{stored property 'x' without initial value prevents synthesized initializers}}
}

func testDefaultInitializers() {
  _ = DefaultInitializerStruct()
  _ = DefaultInitializerClass()
  _ = NoDefaultInitializerStruct() // expected-error{{missing argument for parameter 'x' in call}}
}

struct DefaultedPrivateMemberwiseLets {
  @Wrapper(value: true)
  private var x: Bool

  @WrapperWithInitialValue
  var y: Int = 17

  @WrapperWithInitialValue(initialValue: 17)
  private var z: Int
}

func testDefaultedPrivateMemberwiseLets() {
  _ = DefaultedPrivateMemberwiseLets()
  _ = DefaultedPrivateMemberwiseLets(y: 42)
  _ = DefaultedPrivateMemberwiseLets(x: Wrapper(value: false)) // expected-error{{incorrect argument label in call (have 'x:', expected 'y:')}}
}


// ---------------------------------------------------------------------------
// Storage references
// ---------------------------------------------------------------------------
@propertyWrapper
struct WrapperWithStorageRef<T> {
  var value: T

  var wrapperValue: Wrapper<T> {
    return Wrapper(value: value)
  }
}

extension Wrapper {
  var wrapperOnlyAPI: Int { return 17 }
}

struct TestStorageRef {
  @WrapperWithStorageRef var x: Int // expected-note{{'$$x' declared here}}
  // expected-note@-1{{'$x' declared here}}

  init(x: Int) {
    self.$$x = WrapperWithStorageRef(value: x)
  }

  mutating func test() {
    let _: Wrapper = $x
    let i = $x.wrapperOnlyAPI
    let _: Int = i

    // x is mutable, $x is not
    x = 17
    $x = Wrapper(value: 42) // expected-error{{cannot assign to property: '$x' is immutable}}
  }
}

func testStorageRef(tsr: TestStorageRef) {
  let _: Wrapper = tsr.$x // expected-error{{'$x' is inaccessible due to 'private' protection level}}
  _ = tsr.$$x // expected-error{{'$$x' is inaccessible due to 'private' protection level}}
}

// rdar://problem/50873275 - crash when using wrapper with wrapperValue in
// generic type.
@propertyWrapper
struct InitialValueWrapperWithStorageRef<T> {
  var value: T

  init(initialValue: T) {
    value = initialValue
  }

  var wrapperValue: Wrapper<T> {
    return Wrapper(value: value)
  }
}

struct TestGenericStorageRef<T> {
  struct Inner { }
  @InitialValueWrapperWithStorageRef var inner: Inner = Inner()
}

// ---------------------------------------------------------------------------
// Misc. semantic issues
// ---------------------------------------------------------------------------
@propertyWrapper
struct BrokenLazy { }
// expected-error@-1{{property wrapper type 'BrokenLazy' does not contain a non-static property named 'value'}}
// expected-note@-2{{'BrokenLazy' declared here}}

struct S {
  @BrokenLazy // expected-error{{struct 'BrokenLazy' cannot be used as an attribute}}
  var value: Int
}

// ---------------------------------------------------------------------------
// Closures in initializers
// ---------------------------------------------------------------------------
struct UsesExplicitClosures {
  @WrapperAcceptingAutoclosure(body: { 42 })
  var x: Int

  @WrapperAcceptingAutoclosure(body: { return 42 })
  var y: Int
}

// ---------------------------------------------------------------------------
// Miscellaneous bugs
// ---------------------------------------------------------------------------

// rdar://problem/50822051 - compiler assertion / hang
@propertyWrapper
struct PD<Value> {
  var value: Value

  init<A>(initialValue: Value, a: A) { // expected-note{{'init(initialValue:a:)' declared here}}
    self.value = initialValue
  }
}

struct TestPD {
  @PD(a: "foo") var foo: Int = 42 // expected-error{{property 'foo' with attached wrapper cannot initialize both the wrapper type and the property}}
  // expected-error@-1{{missing argument for parameter 'initialValue' in call}}
}

protocol P { }

@propertyWrapper
struct WrapperRequiresP<T: P> {
  var value: T
  var wrapperValue: T { return value }
}

struct UsesWrapperRequiringP {
  // expected-note@-1{{in declaration of}}
  
  @WrapperRequiresP var x.: UsesWrapperRequiringP
  // expected-error@-1{{expected member name following '.'}}
  // expected-error@-2{{expected declaration}}
  // expected-error@-3{{type annotation missing in pattern}}
}

// SR-10899 / rdar://problem/51588022
@propertyWrapper
struct SR_10899_Wrapper { // expected-note{{property wrapper type 'SR_10899_Wrapper' declared here}}
  var value: String { "hi" }
}

struct SR_10899_Usage {
  @SR_10899_Wrapper var thing: Bool // expected-error{{property type 'Bool' does not match that of the 'value' property of its wrapper type 'SR_10899_Wrapper'}}
}

// ---------------------------------------------------------------------------
// Property wrapper composition
// ---------------------------------------------------------------------------
@propertyWrapper
struct WrapperA<Value> {
  var value: Value

  init(initialValue: Value) {
    value = initialValue
  }
}

@propertyWrapper
struct WrapperB<Value> {
  var value: Value

  init(initialValue: Value) {
    value = initialValue
  }
}

@propertyWrapper
struct WrapperC<Value> {
  var value: Value?

  init(initialValue: Value?) {
    value = initialValue
  }
}

struct TestComposition {
  @WrapperA @WrapperB @WrapperC var p1: Int?
  @WrapperA @WrapperB @WrapperC var p2 = "Hello"

	func triggerErrors(d: Double) {
		p1 = d // expected-error{{cannot assign value of type 'Double' to type 'Int?'}}
		p2 = d // expected-error{{cannot assign value of type 'Double' to type 'String?'}}

		$p1 = d // expected-error{{cannot assign value of type 'Double' to type 'WrapperA<WrapperB<WrapperC<Int>>>'}}
		$p2 = d // expected-error{{cannot assign value of type 'Double' to type 'WrapperA<WrapperB<WrapperC<String>>>'}}
	}
}
