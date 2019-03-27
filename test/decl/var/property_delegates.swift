// RUN: %target-typecheck-verify-swift -swift-version 5

@propertyDelegate
struct Wrapper<T> { // expected-note{{generic struct 'Wrapper' declared here}}
  var value: T
}

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

func testParsing() {
  var wrapped1: Int by Wrapper
  var wrapped2: Int by Wrapper

  _ = wrapped1
  _ = wrapped2
}

func testParseError() {
  let (a, b): (Int, Int) by Wrapper // expected-error{{property delegate can only by written on a single-variable pattern}}
  let (c, d): (Int, Int) by // expected-error{{expected property delegate type after 'by'}}

  _ = a
  _ = b
  _ = c
  _ = d
}

// ---------------------------------------------------------------------------
// Type formation
// ---------------------------------------------------------------------------
func testExplicitWrapperType() {
  var wrapped1: Int by Wrapper

  wrapped1 = "Hello" // expected-error{{cannot assign value of type 'String' to type 'Int'}}
}

@propertyDelegate
struct NonGenericWrapper { }
// expected-error@-1{{property delegate type must have a single generic type parameter}}

@propertyDelegate
struct TwoParameterWrapper<T, U> { }
// expected-error@-1{{property delegate type must have a single generic type parameter}}

@propertyDelegate
struct MissingValue<T> { }
// expected-error@-1{{property delegate type 'MissingValue' does not contain a non-static property named 'value'}}

// expected-note@+1{{type 'NotADelegate<T>' must have the attribute '@propertyDelegate' to be used as a property delegate}}{{1-1=@propertyDelegate}}
struct NotADelegate<T> {
  var value: T
}

// expected-error@+1{{'@propertyDelegate' attribute cannot be applied to this declaration}}
@propertyDelegate
protocol CannotBeADelegate {
  associatedtype Value
  var value: Value { get set }
}

func testBadWrapperTypes() {
  var wrapped1: Int by NonGenericWrapper // expected-error{{property delegate type 'NonGenericWrapper' must not provide generic arguments}}
  var wrapped2: Int by TwoParameterWrapper
  var wrapped3: Int by TwoParameterWrapper<Int, Int> // expected-error{{property delegate type 'TwoParameterWrapper<Int, Int>' must not provide generic arguments}}
  var wrapped4: Int by (Int) // expected-error{{use of non-property delegate type 'Int' as a property delegate}}
  var wrapped5: Int by Wrapper<Int> // expected-error{{property delegate type 'Wrapper<Int>' must not provide generic arguments}}
  var wrapped6: Int by NotADelegate // expected-error{{use of non-property delegate type 'NotADelegate' as a property delegate}}

  wrapped1 = 0
  wrapped2 = 0
  wrapped3 = 0
  wrapped4 = 0
  wrapped5 = 0
  wrapped6 = 0
  _ = wrapped1
  _ = wrapped2
  _ = wrapped3
  _ = wrapped4
  _ = wrapped5
  _ = wrapped6
}

// ---------------------------------------------------------------------------
// Property delegates as members
// ---------------------------------------------------------------------------
struct HasDelegate {
  var wrapped1: Int by Wrapper
}

// ---------------------------------------------------------------------------
// Initial value initializers
// ---------------------------------------------------------------------------
@propertyDelegate
struct WrapperWithInitialValue<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

func testInitialValueInference(i: Int, s: String) {
  // Inferring the type of the property itself
  var x by WrapperWithInitialValue = i
  x = 3.14159 // expected-error{{cannot assign value of type 'Double' to type 'Int'}}

  // Inferring part of the type of the property itself
  var y: Dictionary by WrapperWithInitialValue = [s: i]
  y = 3.14159 // expected-error{{cannot assign value of type 'Double' to type 'Dictionary<String, Int>'}}
}

func testInitialValueWithoutDelegateSupport(i: Int) {
  var x by Wrapper = i // expected-error{{initializing property 'x' with delegate 'Wrapper' that lacks an 'init(initialValue:)' initializer; use (...) instead}}
}

@propertyDelegate
struct WrapperWithAmbiguousInitialValue<T> { // expected-error{{property delegate type 'WrapperWithAmbiguousInitialValue' has multiple initial-value initializers}}
  var value: T

  init(initialValue: T?) { // expected-note{{initializer 'init(initialValue:)' declared here}}
    self.value = initialValue!
  }

  init(initialValue: T) { // expected-note{{initializer 'init(initialValue:)' declared here}}
    self.value = initialValue
  }
}

extension Wrapper {
  init(name: String, value: T) {
    self.value = value
  }
}

func testDirectDelegateInitialization(s: String, i: Int) {
  var x by Wrapper(value: i)
  x = 3.14159 // expected-error{{cannot assign value of type 'Double' to type 'Int'}}

  var y by Wrapper(name: "Hello", value: 3.14159)
  y = "hello" // expected-error{{cannot assign value of type 'String' to type 'Double'}}

  // FIXME: Diagnostic below should say "specified type 'Wrapper<Int>'.
  var z: Int by Wrapper(name: "Hello", value: 3.14159)
  // expected-error@-1{{cannot convert value of type 'Wrapper<Double>' to specified type 'Int'}}
}

// ---------------------------------------------------------------------------
// @autoclosure initializers
// ---------------------------------------------------------------------------
@propertyDelegate
struct WrapperAcceptingAutoclosure<T> {
  private let fn: () -> T

  var value: T {
    return fn()
  }

  init(initialValue fn: @autoclosure @escaping () -> T) {
    self.fn = fn
  }
}

func seventeen() -> Int { return 17 }

struct UseWrapperAcceptingAutoclosure {
  var foo by WrapperAcceptingAutoclosure = seventeen()
}

// ---------------------------------------------------------------------------
// Memberwise initializers
// ---------------------------------------------------------------------------
struct MemberwiseInits<T> {
  var x: Bool by Wrapper
  var y: T by WrapperWithInitialValue
}

func testMemberwiseInits() {
  // expected-error@+1{{type '(Wrapper<Bool>, Double) -> MemberwiseInits<Double>'}}
  let _: Int = MemberwiseInits<Double>.init

  _ = MemberwiseInits(x: Wrapper(value: true), y: 17)
}

struct DefaultedMemberwiseInits {
  var x: Bool by Wrapper(value: true)
  var y: Int by WrapperWithInitialValue = 17
}

func testDefaultedMemberwiseInits() {
  // FIXME: We would like defaults for these, but it is not implemented yet.
  _ = DefaultedMemberwiseInits()
  _ = DefaultedMemberwiseInits(x: Wrapper(value: false), y: 42)

  _ = DefaultedMemberwiseInits(y: 42)
  // expected-error@-1{{cannot invoke initializer for type 'DefaultedMemberwiseInits' with an argument list of type '(y: Int)'}}
  // expected-note@-2{{overloads for 'DefaultedMemberwiseInits' exist with these partially matching parameter lists: (), (x: Wrapper<Bool>, y: Int)}}
  _ = DefaultedMemberwiseInits(x: Wrapper(value: false))
  // expected-error@-1{{cannot invoke initializer for type 'DefaultedMemberwiseInits' with an argument list of type '(x: Wrapper<Bool>)'}}
  // expected-note@-2{{overloads for 'DefaultedMemberwiseInits' exist with these partially matching parameter lists: (), (x: Wrapper<Bool>, y: Int)}}
}

// ---------------------------------------------------------------------------
// Default initializers
// ---------------------------------------------------------------------------
struct DefaultInitializerStruct {
  var x by Wrapper(value: true)
  var y: Int by WrapperWithInitialValue = 10
}

struct NoDefaultInitializerStruct { // expected-note{{'init(x:)' declared here}}
  var x: Bool by Wrapper
}

class DefaultInitializerClass {
  final var x by Wrapper(value: true)
  final var y: Int by WrapperWithInitialValue = 10
}

class NoDefaultInitializerClass { // expected-error{{class 'NoDefaultInitializerClass' has no initializers}}
  // FIXME: Reference 'x' instead of '$x' here
  final var x: Bool by Wrapper  // expected-note{{stored property '$x' without initial value prevents synthesized initializers}}
}

func testDefaultInitializers() {
  _ = DefaultInitializerStruct()
  _ = DefaultInitializerClass()
  _ = NoDefaultInitializerStruct() // expected-error{{missing argument for parameter 'x' in call}}
}

// ---------------------------------------------------------------------------
// Referencing the backing store
// ---------------------------------------------------------------------------
extension MemberwiseInits {
  func getXStorage() -> Wrapper<Bool> {
    return $x
  }

  func getYStorage() -> WrapperWithInitialValue<T> {
    return self.$y
  }
}

// ---------------------------------------------------------------------------
// Nested delegates
// ---------------------------------------------------------------------------
struct HasNestedDelegate<T> {
  @propertyDelegate
  struct NestedDelegate<U> {
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  var y: [T] by NestedDelegate = []
}

struct UsesNestedDelegate<V> {
  var y: [V] by HasNestedDelegate<V>.NestedDelegate
}

// ---------------------------------------------------------------------------
// Access control
// ---------------------------------------------------------------------------
struct HasPrivateDelegate<T> {
  @propertyDelegate
  private struct PrivateDelegate<U> { // expected-note{{type declared here}}
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  var y: [T] by PrivateDelegate = []
  // expected-error@-1{{property must be declared private because its property delegate type uses a private type}}

  // Okay to reference private entities from a private delegate instance
  var z: [T] by private PrivateDelegate
}

public struct HasUsableFromInlineDelegate<T> {
  @propertyDelegate
  struct InternalDelegate<U> { // expected-note{{type declared here}}
    var value: U
    init(initialValue: U) {
      self.value = initialValue
    }
  }

  @usableFromInline
  var y: [T] by InternalDelegate = []
  // expected-error@-1{{property delegate type referenced from a '@usableFromInline' property must be '@usableFromInline' or public}}
}

@propertyDelegate
class Box<Value> {
  private(set) var value: Value

  init(initialValue: Value) {
    self.value = initialValue
  }
}

struct UseBox {
  var x by Box = 17
  var y: Int by Box {
    set { }
  }
}

func testBox(ub: UseBox) {
  _ = ub.x
  ub.x = 5 // expected-error{{cannot assign to property: 'x' is a get-only property}}

  _ = ub.y
  ub.y = 20 // expected-error{{cannot assign to property: 'ub' is a 'let' constant}}

  var mutableUB = ub
  _ = mutableUB.y
  mutableUB.y = 20
  mutableUB = ub
}

// ---------------------------------------------------------------------------
// Access control for the delegate instance
// ---------------------------------------------------------------------------
struct HasPrivateDelegateInstance<T> {
  var x: [T] by fileprivate WrapperWithInitialValue = []
  var y: [T] by private WrapperWithInitialValue = [] // expected-note{{'$y' declared here}}
  var z: [T] by public WrapperWithInitialValue = [] // expected-error{{specified property delegate access 'public' cannot be greater than that of the original property ('internal')}}
  private(set) var w: [T] by WrapperWithInitialValue = []

  func getStorage(which: Bool) -> WrapperWithInitialValue<[T]> {
    return which ? $x : $y
  }
}

func testPrivateDelegateInstance(p: HasPrivateDelegateInstance<Int>) {
  _ = p.$x
  _ = p.$y // expected-error{{'$y' is inaccessible due to 'private' protection level}}
  _ = p.$w

  var mutableP = p
  mutableP.$w = p.$w // expected-error{{cannot assign to property: '$w' is immutable}}
}

// ---------------------------------------------------------------------------
// Access control for the memberwise initializer
// ---------------------------------------------------------------------------
struct NotPrivateMemberwiseInits<T> {
  var x: Bool by Wrapper
  var y: T by private WrapperWithInitialValue

  static func construct(x: Wrapper<Bool>, y: T)
      -> NotPrivateMemberwiseInits<T> {
    return NotPrivateMemberwiseInits<T>(x: x, y: y)
  }
}

struct PrivateMemberwiseInits<T> { // expected-note{{'init(x:y:)' declared here}}
  var x: Bool by private Wrapper
  var y: T by WrapperWithInitialValue

  static func construct(x: Wrapper<Bool>, y: T)
      -> PrivateMemberwiseInits<T> {
    return PrivateMemberwiseInits<T>(x: x, y: y)
  }
}

func testMemberInitAccess<T>(x: Wrapper<Bool>, y: T) {
  _ = NotPrivateMemberwiseInits<T>(x: x, y: y)

  // expected-error@+1{{'PrivateMemberwiseInits<T>' initializer is inaccessible due to 'private' protection level}}
  _ = PrivateMemberwiseInits<T>(x: x, y: y)
}

// ---------------------------------------------------------------------------
// Miscellaneous restrictions
// ---------------------------------------------------------------------------
enum SomeEnum {
  case foo

  var bar: Int by Wrapper(value: 17) // expected-error{{property 'bar' declared inside an enum cannot have a delegate}}
  // expected-error@-1{{cannot convert value of type '(value: Int)' to specified type 'Int'}}
  
  static var x by Wrapper(value: 17) // okay
}

protocol SomeProtocol {
  var bar: Int by Wrapper(value: 17) // expected-error{{property 'bar' declared inside a protocol cannot have a delegate}}
  // expected-error@-1{{initial value is not allowed here}}
  
  static var x: Int by Wrapper(value: 17) // expected-error{{property 'x' declared inside a protocol cannot have a delegate}}
  // expected-error@-1{{static stored properties not supported in generic types}}
  // expected-error@-2{{static stored properties not supported in generic types}}
  // expected-error@-3{{initial value is not allowed here}}
}

extension HasDelegate {
  var inExt: Int by Wrapper(value: 17) // expected-error{{property 'inExt' declared inside an extension cannot have a delegate}}
  // expected-error@-1{{cannot convert value of type '(value: Int)' to specified type 'Int'}}

  static var x by Wrapper(value: 17) // okay
}

class ClassWithDelegates {
  final var x by Wrapper(value: 17)
  var y by Wrapper(value: 17) // expected-error{{property 'y' with a delegate must be 'final'}}{{3-3=final}}
}

final class FinalClassWithDelegates {
  var x by Wrapper(value: 17)
}

class Superclass {
  var x: Int = 0
}

class SubclassWithDelegate: Superclass {
  final override var x by Wrapper(value: 17) // expected-error{{property 'x' with a delegate cannot override another property}}
}

class C { }

struct BadCombinations {
  lazy var x: C by WrapperWithInitialValue = C() // expected-error{{property 'x' with a delegate cannot also be lazy}}
  weak var y: C? by Wrapper // expected-error{{property 'y' with a delegate cannot also be weak}}
  unowned var z: C by Wrapper // expected-error{{property 'z' with a delegate cannot also be unowned}}
}

@propertyDelegate
struct NonVisibleValueDelegate<Value> {
  private var value: Value // expected-error{{private property 'value' cannot have more restrictive access than its enclosing property delegate type 'NonVisibleValueDelegate' (which is internal)}}
}

@propertyDelegate
struct NonVisibleInitDelegate<Value> {
  var value: Value

  private init(initialValue: Value) { // expected-error{{private initializer 'init(initialValue:)' cannot have more restrictive access than its enclosing property delegate type 'NonVisibleInitDelegate' (which is internal)}}
    self.value = initialValue
  }
}

// ---------------------------------------------------------------------------
// Explicitly-specified accessors
// ---------------------------------------------------------------------------

struct DelegateWithAccessors { // expected-note{{'init(x:y:)' declared here}}
  var x: Int by Wrapper {
    return $x.value * 2
  }

  var y: Int by WrapperWithInitialValue {
    set {
      $y.value = newValue / 2
    }
  }
}

func testDelegateWithAccessors() {
  _ = DelegateWithAccessors() // expected-error{{missing argument for parameter 'x' in call}}
  _ = DelegateWithAccessors(x: Wrapper<Int>(value: 17), y: 42)
}

// ---------------------------------------------------------------------------
// Mutating/nonmutating
// ---------------------------------------------------------------------------
@propertyDelegate
struct DelegateWithNonMutatingSetter<Value> {
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

@propertyDelegate
struct DelegateWithMutatingGetter<Value> {
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

struct UseMutatingnessDelegates {
  var x by DelegateWithNonMutatingSetter = true
  var y by DelegateWithMutatingGetter = 17
}

func testMutatingness() {
  var mutable = UseMutatingnessDelegates()

  _ = mutable.x
  mutable.x = false

  _ = mutable.y
  mutable.y = 42

  let nonmutable = UseMutatingnessDelegates() // expected-note 2{{change 'let' to 'var' to make it mutable}}

  // Okay due to nonmutating setter
  _ = nonmutable.x
  nonmutable.x = false

  _ = nonmutable.y // expected-error{{cannot use mutating getter on immutable value: 'nonmutable' is a 'let' constant}}
  nonmutable.y = 42 // expected-error{{cannot use mutating getter on immutable value: 'nonmutable' is a 'let' constant}}
}
