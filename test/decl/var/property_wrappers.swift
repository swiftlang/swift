// RUN: %target-typecheck-verify-swift -swift-version 5

// ---------------------------------------------------------------------------
// Property wrapper type definitions
// ---------------------------------------------------------------------------
@propertyWrapper
struct Wrapper<T> {
  private var _stored: T
  init(stored: T) {
    self._stored = stored
  }
  
  var wrappedValue: T {
    get { _stored }
    set { _stored = newValue }
  }
  
}

@propertyWrapper
struct WrapperWithInitialValue<T> {
  var wrappedValue: T

  init(wrappedValue initialValue: T) {
    self.wrappedValue = initialValue
  }
}

@propertyWrapper
struct WrapperWithDefaultInit<T> {
  private var stored: T?

  var wrappedValue: T {
    get { stored! }
    set { stored = newValue }
  }

  init() {
    self.stored = nil
  }
}

@propertyWrapper
struct WrapperAcceptingAutoclosure<T> {
  private let fn: () -> T

  var wrappedValue: T {
    return fn()
  }

  init(wrappedValue fn: @autoclosure @escaping () -> T) {
    self.fn = fn
  }

  init(body fn: @escaping () -> T) {
    self.fn = fn
  }
}

@propertyWrapper
struct MissingValue<T> { }
// expected-error@-1{{property wrapper type 'MissingValue' does not contain a non-static property named 'wrappedValue'}}

@propertyWrapper
struct StaticValue {
  static var wrappedValue: Int = 17
}
// expected-error@-3{{property wrapper type 'StaticValue' does not contain a non-static property named 'wrappedValue'}}


// expected-error@+1{{'@propertyWrapper' attribute cannot be applied to this declaration}}
@propertyWrapper
protocol CannotBeAWrapper {
  associatedtype Value
  var wrappedValue: Value { get set }
}

@propertyWrapper
struct NonVisibleValueWrapper<Value> {
  private var wrappedValue: Value // expected-error{{private property 'wrappedValue' cannot have more restrictive access than its enclosing property wrapper type 'NonVisibleValueWrapper' (which is internal)}}
}

@propertyWrapper
struct NonVisibleInitWrapper<Value> {
  var wrappedValue: Value

  private init(wrappedValue initialValue: Value) { // expected-error{{private initializer 'init(wrappedValue:)' cannot have more restrictive access than its enclosing property wrapper type 'NonVisibleInitWrapper' (which is internal)}}
    self.wrappedValue = initialValue
  }
}

@propertyWrapper
struct InitialValueTypeMismatch<Value> {
  var wrappedValue: Value // expected-note{{'wrappedValue' declared here}}

  init(wrappedValue initialValue: Value?) { // expected-error{{'init(wrappedValue:)' parameter type ('Value?') must be the same as its 'wrappedValue' property type ('Value') or an @autoclosure thereof}}
    self.wrappedValue = initialValue!
  }
}

@propertyWrapper
struct MultipleInitialValues<Value> { // expected-error{{property wrapper type 'MultipleInitialValues' has multiple initial-value initializers}}
  var wrappedValue: Value? = nil

  init(wrappedValue initialValue: Int) { // expected-note{{initializer 'init(wrappedValue:)' declared here}}
  }

  init(wrappedValue initialValue: Double) { // expected-note{{initializer 'init(wrappedValue:)' declared here}}
  }
}

@propertyWrapper
struct InitialValueFailable<Value> {
  var wrappedValue: Value

  init?(wrappedValue initialValue: Value) { // expected-error{{'init(wrappedValue:)' cannot be failable}}
    return nil
  }
}

@propertyWrapper
struct InitialValueFailableIUO<Value> {
  var wrappedValue: Value

  init!(wrappedValue initialValue: Value) {  // expected-error{{'init(wrappedValue:)' cannot be failable}}
    return nil
  }
}

// ---------------------------------------------------------------------------
// Property wrapper type definitions
// ---------------------------------------------------------------------------
@propertyWrapper
struct _lowercaseWrapper<T> {
  var wrappedValue: T
}

@propertyWrapper
struct _UppercaseWrapper<T> {
  var wrappedValue: T
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

  @Wrapper(stored: 17)
  var bar: Int // expected-error{{property 'bar' declared inside an enum cannot have a wrapper}}
  // expected-error@-1{{enums must not contain stored properties}}

  @Wrapper(stored: 17)
  static var x: Int
}

protocol SomeProtocol {
  @Wrapper(stored: 17)
  var bar: Int // expected-error{{property 'bar' declared inside a protocol cannot have a wrapper}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}

  @Wrapper(stored: 17)
  static var x: Int // expected-error{{property 'x' declared inside a protocol cannot have a wrapper}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}
}

struct HasWrapper { }

extension HasWrapper {
  @Wrapper(stored: 17)
  var inExt: Int // expected-error{{property 'inExt' declared inside an extension cannot have a wrapper}}
  // expected-error@-1{{extensions must not contain stored properties}}

  @Wrapper(stored: 17)
  static var x: Int
}

class ClassWithWrappers {
  @Wrapper(stored: 17)
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
  @Wrapper(stored: 17)
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
  @Wrapper(stored: 17)
  @WrapperWithInitialValue // expected-error{{extra argument 'wrappedValue' in call}}
  var x: Int = 17

  @WrapperWithInitialValue // expected-error 2{{property wrapper can only apply to a single variable}}
  var (y, z) = (1, 2)
}

// ---------------------------------------------------------------------------
// Initialization
// ---------------------------------------------------------------------------

struct Initialization {
  @Wrapper(stored: 17)
  var x: Int

  @Wrapper(stored: 17)
  var x2: Double

  @Wrapper(stored: 17)
  var x3 = 42 // expected-error{{extra argument 'wrappedValue' in call}}

  @Wrapper(stored: 17)
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

@propertyWrapper
struct Clamping<V: Comparable> {
  var value: V
  let min: V
  let max: V

  init(wrappedValue initialValue: V, min: V, max: V) {
    value = initialValue
    self.min = min
    self.max = max
    assert(value >= min && value <= max)
  }

  var wrappedValue: V {
    get { return value }
    set {
      if newValue < min {
        value = min
      } else if newValue > max {
        value = max
      } else {
        value = newValue
      }
    }
  }
}

struct Color {
  @Clamping(min: 0, max: 255) var red: Int = 127
  @Clamping(min: 0, max: 255) var green: Int = 127
  @Clamping(min: 0, max: 255) var blue: Int = 127
  @Clamping(min: 0, max: 255) var alpha: Int = 255
}

func testColor() {
  _ = Color(green: 17)
}

// ---------------------------------------------------------------------------
// Wrapper type formation
// ---------------------------------------------------------------------------
@propertyWrapper
struct IntWrapper {
  var wrappedValue: Int
}

@propertyWrapper
struct WrapperForHashable<T: Hashable> { // expected-note{{property wrapper type 'WrapperForHashable' declared here}}
  var wrappedValue: T
}

@propertyWrapper
struct WrapperWithTwoParams<T, U> {
  var wrappedValue: (T, U)
}

struct NotHashable { }

struct UseWrappersWithDifferentForm {
  @IntWrapper
  var x: Int

  // FIXME: Diagnostic should be better here
  @WrapperForHashable
  var y: NotHashable // expected-error{{property type 'NotHashable' does not match that of the 'wrappedValue' property of its wrapper type 'WrapperForHashable'}}

  @WrapperForHashable
  var yOkay: Int

  @WrapperWithTwoParams
  var zOkay: (Int, Float)

  // FIXME: Need a better diagnostic here
  @HasNestedWrapper.NestedWrapper
  var w: Int // expected-error{{property type 'Int' does not match that of the 'wrappedValue' property of its wrapper type 'HasNestedWrapper.NestedWrapper'}}

  @HasNestedWrapper<Double>.NestedWrapper
  var wOkay: Int

  @HasNestedWrapper.ConcreteNestedWrapper
  var wOkay2: Int
}

@propertyWrapper
struct Function<T, U> { // expected-note{{property wrapper type 'Function' declared here}}
  var wrappedValue: (T) -> U?
}

struct TestFunction {
  @Function var f: (Int) -> Float?
  
  @Function var f2: (Int) -> Float // expected-error{{property type '(Int) -> Float' does not match that of the 'wrappedValue' property of its wrapper type 'Function'}}

  func test() {
    let _: Int = _f // expected-error{{cannot convert value of type 'Function<Int, Float>' to specified type 'Int'}}
  }
}

// ---------------------------------------------------------------------------
// Nested wrappers
// ---------------------------------------------------------------------------
struct HasNestedWrapper<T> {
  @propertyWrapper
  struct NestedWrapper<U> { // expected-note{{property wrapper type 'NestedWrapper' declared here}}
    var wrappedValue: U
    init(wrappedValue initialValue: U) {
      self.wrappedValue = initialValue
    }
  }

  @propertyWrapper
  struct ConcreteNestedWrapper {
    var wrappedValue: T
    init(wrappedValue initialValue: T) {
      self.wrappedValue = initialValue
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
    return _x
  }

  func getYStorage() -> WrapperWithInitialValue<Bool> {
    return self._y
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
    var wrappedValue: Value
    init(wrappedValue: Value) {
      self.wrappedValue = wrappedValue
    }
  }

  var box: Box

  init(wrappedValue initialValue: Value) {
    self.box = Box(wrappedValue: initialValue)
  }

  var wrappedValue: Value {
    get { return box.wrappedValue }
    nonmutating set { box.wrappedValue = newValue }
  }
}

@propertyWrapper
struct WrapperWithMutatingGetter<Value> {
  var readCount = 0
  var writeCount = 0
  var stored: Value

  init(wrappedValue initialValue: Value) {
    self.stored = initialValue
  }

  var wrappedValue: Value {
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
  var wrappedValue: Value

  init(wrappedValue initialValue: Value) {
    self.wrappedValue = initialValue
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
    var wrappedValue: U
    init(wrappedValue initialValue: U) {
      self.wrappedValue = initialValue
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
    var wrappedValue: U
    init(wrappedValue initialValue: U) {
      self.wrappedValue = initialValue
    }
  }

  @InternalWrapper
  @usableFromInline
  var y: [T] = []
  // expected-error@-1{{property wrapper type referenced from a '@usableFromInline' property must be '@usableFromInline' or public}}
}

@propertyWrapper
class Box<Value> {
  private(set) var wrappedValue: Value

  init(wrappedValue initialValue: Value) {
    self.wrappedValue = initialValue
  }
}

struct UseBox {
  @Box
  var x = 17 // expected-note{{'_x' declared here}}
}

func testBox(ub: UseBox) {
  _ = ub.x
  ub.x = 5 // expected-error{{cannot assign to property: 'x' is a get-only property}}

  var mutableUB = ub
  mutableUB = ub
}

func backingVarIsPrivate(ub: UseBox) {
  _ = ub._x // expected-error{{'_x' is inaccessible due to 'private' protection level}}
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

  _ = MemberwiseInits(x: Wrapper(stored: true), y: 17)
}

struct DefaultedMemberwiseInits {
  @Wrapper(stored: true)
  var x: Bool

  @WrapperWithInitialValue
  var y: Int = 17

  @WrapperWithInitialValue(wrappedValue: 17)
  var z: Int

  @WrapperWithDefaultInit
  var w: Int

  @WrapperWithDefaultInit
  var optViaDefaultInit: Int?

  @WrapperWithInitialValue
  var optViaInitialValue: Int?
}


struct CannotDefaultMemberwiseOptionalInit { // expected-note{{'init(x:)' declared here}}
  @Wrapper
  var x: Int?
}

func testDefaultedMemberwiseInits() {
  _ = DefaultedMemberwiseInits()
  _ = DefaultedMemberwiseInits(
    x: Wrapper(stored: false),
    y: 42,
    z: WrapperWithInitialValue(wrappedValue: 42))

  _ = DefaultedMemberwiseInits(y: 42)
  _ = DefaultedMemberwiseInits(x: Wrapper(stored: false))
  _ = DefaultedMemberwiseInits(z: WrapperWithInitialValue(wrappedValue: 42))
  _ = DefaultedMemberwiseInits(w: WrapperWithDefaultInit())
  _ = DefaultedMemberwiseInits(optViaDefaultInit: WrapperWithDefaultInit())
  _ = DefaultedMemberwiseInits(optViaInitialValue: nil)
  _ = DefaultedMemberwiseInits(optViaInitialValue: 42)

  _ = CannotDefaultMemberwiseOptionalInit() // expected-error{{missing argument for parameter 'x' in call}}
  _ = CannotDefaultMemberwiseOptionalInit(x: Wrapper(stored: nil))
}

// ---------------------------------------------------------------------------
// Default initializers
// ---------------------------------------------------------------------------
struct DefaultInitializerStruct {
  @Wrapper(stored: true)
  var x

  @WrapperWithInitialValue
  var y: Int = 10
}

struct NoDefaultInitializerStruct { // expected-note{{'init(x:)' declared here}}
  @Wrapper
  var x: Bool
}

class DefaultInitializerClass {
  @Wrapper(stored: true)
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
  @Wrapper(stored: true)
  private var x: Bool

  @WrapperWithInitialValue
  var y: Int = 17

  @WrapperWithInitialValue(wrappedValue: 17)
  private var z: Int
}

func testDefaultedPrivateMemberwiseLets() {
  _ = DefaultedPrivateMemberwiseLets()
  _ = DefaultedPrivateMemberwiseLets(y: 42)
  _ = DefaultedPrivateMemberwiseLets(x: Wrapper(stored: false)) // expected-error{{incorrect argument label in call (have 'x:', expected 'y:')}}
}


// ---------------------------------------------------------------------------
// Storage references
// ---------------------------------------------------------------------------
@propertyWrapper
struct WrapperWithStorageRef<T> {
  var wrappedValue: T

  var projectedValue: Wrapper<T> {
    return Wrapper(stored: wrappedValue)
  }
}

extension Wrapper {
  var wrapperOnlyAPI: Int { return 17 }
}

struct TestStorageRef {
  @WrapperWithStorageRef var x: Int // expected-note{{'_x' declared here}}

  init(x: Int) {
    self._x = WrapperWithStorageRef(wrappedValue: x)
  }

  mutating func test() {
    let _: Wrapper = $x
    let i = $x.wrapperOnlyAPI
    let _: Int = i

    // x is mutable, $x is not
    x = 17
    $x = Wrapper(stored: 42) // expected-error{{cannot assign to property: '$x' is immutable}}
  }
}

func testStorageRef(tsr: TestStorageRef) {
  let _: Wrapper = tsr.$x
  _ = tsr._x // expected-error{{'_x' is inaccessible due to 'private' protection level}}
}

struct TestStorageRefPrivate {
  @WrapperWithStorageRef private(set) var x: Int

  init() {
    self._x = WrapperWithStorageRef(wrappedValue: 5)
  }
}

func testStorageRefPrivate() {
  var tsr = TestStorageRefPrivate()
  let a = tsr.$x // okay, getter is internal
  tsr.$x = a  // expected-error{{cannot assign to property: '$x' is immutable}}
}


// rdar://problem/50873275 - crash when using wrapper with projectedValue in
// generic type.
@propertyWrapper
struct InitialValueWrapperWithStorageRef<T> {
  var wrappedValue: T

  init(wrappedValue initialValue: T) {
    wrappedValue = initialValue
  }

  var projectedValue: Wrapper<T> {
    return Wrapper(stored: wrappedValue)
  }
}

struct TestGenericStorageRef<T> {
  struct Inner { }
  @InitialValueWrapperWithStorageRef var inner: Inner = Inner()
}

// Wiring up the _projectedValueProperty attribute.
struct TestProjectionValuePropertyAttr {
  @_projectedValueProperty(wrapperA)
  @WrapperWithStorageRef var a: String

  var wrapperA: Wrapper<String> {
    Wrapper(stored: "blah")
  }

  @_projectedValueProperty(wrapperB) // expected-error{{could not find projection value property 'wrapperB'}}
  @WrapperWithStorageRef var b: String
}


// ---------------------------------------------------------------------------
// Misc. semantic issues
// ---------------------------------------------------------------------------
@propertyWrapper
struct BrokenLazy { }
// expected-error@-1{{property wrapper type 'BrokenLazy' does not contain a non-static property named 'wrappedValue'}}
// expected-note@-2{{'BrokenLazy' declared here}}

struct S {
  @BrokenLazy // expected-error{{struct 'BrokenLazy' cannot be used as an attribute}}
  var wrappedValue: Int
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
  var wrappedValue: Value

  init<A>(wrappedValue initialValue: Value, a: A) {
    self.wrappedValue = initialValue
  }
}

struct TestPD {
  @PD(a: "foo") var foo: Int = 42
}

protocol P { }

@propertyWrapper
struct WrapperRequiresP<T: P> {
  var wrappedValue: T
  var projectedValue: T { return wrappedValue }
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
  var wrappedValue: String { "hi" }
}

struct SR_10899_Usage {
  @SR_10899_Wrapper var thing: Bool // expected-error{{property type 'Bool' does not match that of the 'wrappedValue' property of its wrapper type 'SR_10899_Wrapper'}}
}

// SR-11061 / rdar://problem/52593304 assertion with DeclContext mismatches
class SomeValue {
	@SomeA(closure: { $0 }) var some: Int = 100
}

@propertyWrapper
struct SomeA<T> {
	var wrappedValue: T
	let closure: (T) -> (T)

	init(wrappedValue initialValue: T, closure: @escaping (T) -> (T)) {
		self.wrappedValue = initialValue
		self.closure = closure
	}
}

// rdar://problem/51989272 - crash when the property wrapper is a generic type
// alias
typealias Alias<T> = WrapperWithInitialValue<T>

struct TestAlias {
  @Alias var foo = 17
}



// 
// ---------------------------------------------------------------------------
// Property wrapper composition
// ---------------------------------------------------------------------------
@propertyWrapper
struct WrapperA<Value> {
  var wrappedValue: Value

  init(wrappedValue initialValue: Value) {
    wrappedValue = initialValue
  }
}

@propertyWrapper
struct WrapperB<Value> {
  var wrappedValue: Value

  init(wrappedValue initialValue: Value) {
    wrappedValue = initialValue
  }
}

@propertyWrapper
struct WrapperC<Value> {
  var wrappedValue: Value?

  init(wrappedValue initialValue: Value?) {
    wrappedValue = initialValue
  }
}

@propertyWrapper
struct WrapperD<Value, X, Y> { // expected-note{{property wrapper type 'WrapperD' declared here}}
  var wrappedValue: Value
}

@propertyWrapper
struct WrapperE<Value> {
  var wrappedValue: Value
}

struct TestComposition {
  @WrapperA @WrapperB @WrapperC var p1: Int?
  @WrapperA @WrapperB @WrapperC var p2 = "Hello"
  @WrapperD<WrapperE, Int, String> @WrapperE var p3: Int?
  @WrapperD<WrapperC, Int, String> @WrapperC var p4: Int?
  @WrapperD<WrapperC, Int, String> @WrapperE var p5: Int // expected-error{{property type 'Int' does not match that of the 'wrappedValue' property of its wrapper type 'WrapperD<WrapperC, Int, String>'}}

	func triggerErrors(d: Double) {
		p1 = d // expected-error{{cannot assign value of type 'Double' to type 'Int?'}}
		p2 = d // expected-error{{cannot assign value of type 'Double' to type 'String?'}}
    p3 = d // expected-error{{cannot assign value of type 'Double' to type 'Int?'}}

		_p1 = d // expected-error{{cannot assign value of type 'Double' to type 'WrapperA<WrapperB<WrapperC<Int>>>'}}
		_p2 = d // expected-error{{cannot assign value of type 'Double' to type 'WrapperA<WrapperB<WrapperC<String>>>'}}
    _p3 = d // expected-error{{cannot assign value of type 'Double' to type 'WrapperD<WrapperE<Int?>, Int, String>'}}
	}
}

// ---------------------------------------------------------------------------
// Missing Property Wrapper Unwrap Diagnostics
// ---------------------------------------------------------------------------
@propertyWrapper
struct Foo<T> { // expected-note {{arguments to generic parameter 'T' ('W' and 'Int') are expected to be equal}}
  var wrappedValue: T {
    get {
      fatalError("boom")
    }

    set {
    }
  }

  var prop: Int = 42

  func foo() {}
  func bar(x: Int) {}

  subscript(q: String) -> Int {
    get { return 42 }
    set { }
  }

  subscript(x x: Int) -> Int {
    get { return 42 }
    set { }
  }

  subscript(q q: String, a: Int) -> Bool {
    get { return false }
    set { }
  }
}

@propertyWrapper
struct Bar<T, V> {
  var wrappedValue: T

  func bar() {}

  // TODO(diagnostics): We need to figure out what to do about subscripts.
  // The problem standing in our way - keypath application choice
  // is always added to results even if it's not applicable.
}

@propertyWrapper
struct Baz<T> {
  var wrappedValue: T
  
  func onPropertyWrapper() {}

  var projectedValue: V {
    return V()
  }
}

extension Bar where V == String { // expected-note {{where 'V' = 'Bool'}}
  func barWhereVIsString() {}
}

struct V {
  func onProjectedValue() {}
}

struct W {
  func onWrapped() {}
}

struct MissingPropertyWrapperUnwrap {
  @Foo var w: W
  @Foo var x: Int
  @Bar<Int, Bool> var y: Int
  @Bar<Int, String> var z: Int
  @Baz var usesProjectedValue: W
  
  func a<T>(_: Foo<T>) {}
  func a<T>(named: Foo<T>) {}
  func b(_: Foo<Int>) {}
  func c(_: V) {}
  func d(_: W) {}
  func e(_: Foo<W>) {}

  func baz() {
    self.x.foo() // expected-error {{referencing instance method 'foo()' requires wrapper 'Foo<Int>'}}{{10-10=_}}
    self.x.prop  // expected-error {{referencing property 'prop' requires wrapper 'Foo<Int>'}} {{10-10=_}}
    self.x.bar(x: 42) // expected-error {{referencing instance method 'bar(x:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}
    self.y.bar() // expected-error {{referencing instance method 'bar()' requires wrapper 'Bar<Int, Bool>'}}{{10-10=_}}
    self.y.barWhereVIsString() // expected-error {{referencing instance method 'barWhereVIsString()' requires wrapper 'Bar<Int, Bool>'}}{{10-10=_}}
    // expected-error@-1 {{referencing instance method 'barWhereVIsString()' on 'Bar' requires the types 'Bool' and 'String' be equivalent}}
    self.z.barWhereVIsString() // expected-error {{referencing instance method 'barWhereVIsString()' requires wrapper 'Bar<Int, String>'}}{{10-10=_}}
    self.usesProjectedValue.onPropertyWrapper() // expected-error {{referencing instance method 'onPropertyWrapper()' requires wrapper 'Baz<W>'}}{{10-10=_}}

    self._w.onWrapped() // expected-error {{referencing instance method 'onWrapped()' requires wrapped value of type 'W'}}{{10-11=}}
    self.usesProjectedValue.onProjectedValue() // expected-error {{referencing instance method 'onProjectedValue()' requires wrapper 'V'}}{{10-10=$}}
    self.$usesProjectedValue.onWrapped() // expected-error {{referencing instance method 'onWrapped()' requires wrapped value of type 'W'}}{{10-11=}}
    self._usesProjectedValue.onWrapped() // expected-error {{referencing instance method 'onWrapped()' requires wrapped value of type 'W'}}{{10-11=}}
    
    a(self.w) // expected-error {{cannot convert value 'w' of type 'W' to expected type 'Foo<W>', use wrapper instead}}{{12-12=_}}
    b(self.x) // expected-error {{cannot convert value 'x' of type 'Int' to expected type 'Foo<Int>', use wrapper instead}}{{12-12=_}}
    b(self.w) // expected-error {{cannot convert value of type 'W' to expected argument type 'Foo<Int>'}}
    e(self.w) // expected-error {{cannot convert value 'w' of type 'W' to expected type 'Foo<W>', use wrapper instead}}{{12-12=_}}
    b(self._w) // expected-error {{cannot convert value of type 'Foo<W>' to expected argument type 'Foo<Int>'}}

    c(self.usesProjectedValue) // expected-error {{cannot convert value 'usesProjectedValue' of type 'W' to expected type 'V', use wrapper instead}}{{12-12=$}}
    d(self.$usesProjectedValue) // expected-error {{cannot convert value '$usesProjectedValue' of type 'V' to expected type 'W', use wrapped value instead}}{{12-13=}}
    d(self._usesProjectedValue) // expected-error {{cannot convert value '_usesProjectedValue' of type 'Baz<W>' to expected type 'W', use wrapped value instead}}{{12-13=}}

    self.x["ultimate question"] // expected-error {{referencing subscript 'subscript(_:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}
    self.x["ultimate question"] = 42 // expected-error {{referencing subscript 'subscript(_:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}

    self.x[x: 42] // expected-error {{referencing subscript 'subscript(x:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}
    self.x[x: 42] = 0 // expected-error {{referencing subscript 'subscript(x:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}

    self.x[q: "ultimate question", 42] // expected-error {{referencing subscript 'subscript(q:_:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}
    self.x[q: "ultimate question", 42] = true // expected-error {{referencing subscript 'subscript(q:_:)' requires wrapper 'Foo<Int>'}} {{10-10=_}}
  }
}

struct InvalidPropertyDelegateUse {
  @Foo var x: Int = 42 // expected-error {{cannot invoke initializer for ty}}
  // expected-note@-1{{overloads for 'Foo<_>' exist with these partially matching paramet}}

  func test() {
    self.x.foo() // expected-error {{value of type 'Int' has no member 'foo'}}
  }
}
