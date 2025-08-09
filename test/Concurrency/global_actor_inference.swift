// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/other_global_actor_inference.swiftmodule -module-name other_global_actor_inference -strict-concurrency=complete %S/Inputs/other_global_actor_inference.swift -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -verify-additional-prefix minimal-targeted- -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted -verify-additional-prefix minimal-targeted- -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -I %t -disable-availability-checking %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -verify-additional-prefix complete-tns- -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability

import other_global_actor_inference

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct OtherGlobalActor {
  static let shared = SomeActor()
}

@globalActor
struct GenericGlobalActor<T> {
  static var shared: SomeActor { SomeActor() }
}

// ----------------------------------------------------------------------
// Check that MainActor exists
// ----------------------------------------------------------------------

@MainActor protocol Aluminium {
  func method()
}
@MainActor class Copper {}
@MainActor func iron() {}

struct Carbon {
  @IntWrapper var atomicWeight: Int

  func getWeight() -> Int {
    return atomicWeight
  }
}

// ----------------------------------------------------------------------
// Check that @MainActor(blah) doesn't work
// ----------------------------------------------------------------------
// expected-error@+1{{global actor attribute 'MainActor' cannot have arguments}}
@MainActor(blah) func brokenMainActorAttr() { }

// ----------------------------------------------------------------------
// Global actor inference for protocols
// ----------------------------------------------------------------------

@SomeGlobalActor
protocol P1 {
  func method()
}

protocol P2 {
  @SomeGlobalActor func method1()
  func method2()
}

class C1: P1 {
  func method() { } // expected-note {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from conformance to protocol 'P1'}}

  @OtherGlobalActor func testMethod() {
    method() // expected-error {{call to global actor 'SomeGlobalActor'-isolated instance method 'method()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
    _ = method
  }
}

class C2: P2 {
  func method1() { }  // expected-note {{calls to instance method 'method1()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from conformance to protocol 'P2'}}

  func method2() { }

  @OtherGlobalActor func testMethod() {
    method1() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method1()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
    _ = method1
    method2() // okay
  }
}

struct AllInP1: P1 {
  func method() { } // expected-note {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from conformance to protocol 'P1'}}

  func other() { } // expected-note {{calls to instance method 'other()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from conformance to protocol 'P1'}}
}

func testAllInP1(ap1: AllInP1) { // expected-note 2 {{add '@SomeGlobalActor' to make global function 'testAllInP1(ap1:)' part of global actor 'SomeGlobalActor'}}
  ap1.method() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method()' in a synchronous nonisolated context}}
  ap1.other() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'other()' in a synchronous nonisolated context}}
}

struct NotAllInP1 {
  func other() { }
}

extension NotAllInP1: P1 {
  func method() { } // expected-note{{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from conformance to protocol 'P1'}}
}

func testNotAllInP1(nap1: NotAllInP1) { // expected-note{{add '@SomeGlobalActor' to make global function 'testNotAllInP1(nap1:)' part of global actor 'SomeGlobalActor'}}
  nap1.method() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method()' in a synchronous nonisolated context}}
  nap1.other() // okay
}

// Make sure we don't infer 'nonisolated' for stored properties.
@MainActor
protocol Interface {
  nonisolated var baz: Int { get }
}

// expected-warning@+2{{conformance of 'Object' to protocol 'Interface' crosses into main actor-isolated code and can cause data races}}
@MainActor
class Object: Interface {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}{{15-15=@preconcurrency }}
  // expected-note@-2{{isolate this conformance to the main actor with '@MainActor'}}

  var baz: Int = 42 // expected-note{{main actor-isolated property 'baz' cannot satisfy nonisolated requirement}}
}


// ----------------------------------------------------------------------
// Global actor inference for classes and extensions
// ----------------------------------------------------------------------
@SomeGlobalActor class C3 {
  func method1() { }  // expected-note {{calls to instance method 'method1()' from outside of its actor context are implicitly asynchronous}}
}

extension C3 {
  func method2() { }  // expected-note {{calls to instance method 'method2()' from outside of its actor context are implicitly asynchronous}}
}

class C4: C3 {
  func method3() { }  // expected-note {{calls to instance method 'method3()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from inheritance from class 'C3'}}
}

extension C4 {
  func method4() { }  // expected-note {{calls to instance method 'method4()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-1 {{global actor 'SomeGlobalActor' isolation inferred from inheritance from class 'C3'}}
}

class C5 {
  func method1() { }
}

@SomeGlobalActor extension C5 {
  func method2() { }  // expected-note {{calls to instance method 'method2()' from outside of its actor context are implicitly asynchronous}}
}

@OtherGlobalActor func testGlobalActorInference(c3: C3, c4: C4, c5: C5) {
  // Propagation via class annotation
  c3.method1() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method1()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
  c3.method2() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method2()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
  
  _ = c3.method1
  _ = c3.method2

  // Propagation via subclassing
  c4.method3() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method3()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
  c4.method4() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method4()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}

  _ = c4.method3
  _ = c4.method4

  // Propagation in an extension.
  c5.method1() // OK: no propagation
  c5.method2() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'method2()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}

  _ = c5.method1  // OK
  _ = c5.method2
}

protocol P3 {
  @OtherGlobalActor func method1()
  func method2()
}

class C6: P2, P3 {
  func method1() { }
  func method2() { }

  func testMethod() {
    method1() // okay: no inference
    method2() // okay: no inference
    let _ = method1 // okay: no inference
    let _ = method2 // okay: no inference
  }
}

// ----------------------------------------------------------------------
// Global actor checking for overrides
// ----------------------------------------------------------------------
actor GenericSuper<T> {
  @GenericGlobalActor<T> func method() { } // expected-note {{overridden declaration is here}}

  @GenericGlobalActor<T> func method2() { } // expected-note {{overridden declaration is here}}
  @GenericGlobalActor<T> func method3() { } // expected-note {{overridden declaration is here}}
  @GenericGlobalActor<T> func method4() { }
  @GenericGlobalActor<T> func method5() { }
}

actor GenericSub<T> : GenericSuper<[T]> { // expected-error{{actor types do not support inheritance}}
  override func method() { }  // expected-note {{calls to instance method 'method()' from outside of its actor context are implicitly asynchronous}}
  // expected-error@-1{{instance method overrides a 'final' instance method}}

  @GenericGlobalActor<T> override func method2() { } // expected-error{{instance method overrides a 'final' instance method}}
  nonisolated override func method3() { } // expected-error{{instance method overrides a 'final' instance method}}

  @OtherGlobalActor func testMethod() {
    method() // expected-error{{call to actor-isolated instance method 'method()' in a synchronous global actor 'OtherGlobalActor'-isolated context}}
    _ = method // expected-error{{actor-isolated instance method 'method()' can not be partially applied}}
  }
}

// ----------------------------------------------------------------------
// Global actor inference for superclasses
// ----------------------------------------------------------------------
struct Container<T> {
  @GenericGlobalActor<T> class Superclass { }
  @GenericGlobalActor<[T]> class Superclass2 { }
}

struct OtherContainer<U> {
  // NOT Okay to change the global actor in a subclass.
  @GenericGlobalActor<[U]> class Subclass1 : Container<[U]>.Superclass { }
  @GenericGlobalActor<U> class Subclass2 : Container<[U]>.Superclass { }
  // expected-error@-1{{global actor 'GenericGlobalActor<U>'-isolated class 'Subclass2' has different actor isolation from global actor 'GenericGlobalActor<T>'-isolated superclass 'Superclass'}}

  // Ensure that substitutions work properly when inheriting.
  class Subclass3<V> : Container<(U, V)>.Superclass2 {
    func method() { }

    @OtherGlobalActor func testMethod() async {
      await method()
      let _ = method
    }
  }
}

class SuperclassWithGlobalActors {
  @GenericGlobalActor<Int> func f() { }
  @GenericGlobalActor<Int> func g() { } // expected-note{{overridden declaration is here}}
  func h() { }
  func i() { }
  func j() { }
}

@GenericGlobalActor<String>
class SubclassWithGlobalActors : SuperclassWithGlobalActors {
  override func f() { } // okay: inferred to @GenericGlobalActor<Int>

  @GenericGlobalActor<String> override func g() { } // expected-error{{global actor 'GenericGlobalActor<String>'-isolated instance method 'g()' has different actor isolation from global actor 'GenericGlobalActor<Int>'-isolated overridden declaration}}

  override func h() { } // okay: inferred to unspecified

  func onGenericGlobalActorString() { }
  @GenericGlobalActor<Int> func onGenericGlobalActorInt() { }
}

// ----------------------------------------------------------------------
// Global actor inference for unspecified contexts
// ----------------------------------------------------------------------

// expected-note@+1 {{calls to global function 'foo()' from outside of its actor context are implicitly asynchronous}}
@SomeGlobalActor func foo() { sibling() }

@SomeGlobalActor func sibling() { foo() }

func bar() async {
  // expected-error@+1{{global actor 'SomeGlobalActor'-isolated global function 'foo()' cannot be called from outside of the actor}}{{3-3=await }}
  foo()
}

// expected-note@+1 {{add '@SomeGlobalActor' to make global function 'barSync()' part of global actor 'SomeGlobalActor'}} {{1-1=@SomeGlobalActor }}
func barSync() {
  foo() // expected-error {{call to global actor 'SomeGlobalActor'-isolated global function 'foo()' in a synchronous nonisolated context}}
}

// ----------------------------------------------------------------------
// Property observers
// ----------------------------------------------------------------------

@OtherGlobalActor
struct Observed {
  var thing: Int = 0 {
    didSet {}
    willSet {}
  }
}

func checkObserved(_ o: Observed) {
  _ = o.thing // okay
}

// ----------------------------------------------------------------------
// Property wrappers
// ----------------------------------------------------------------------

@propertyWrapper
@OtherGlobalActor
struct WrapperOnActor<Wrapped: Sendable> {
  private var stored: Wrapped

  nonisolated init(wrappedValue: Wrapped) {
    stored = wrappedValue
  }

  @MainActor var wrappedValue: Wrapped {
    get { }
    set { }
  }

  @SomeGlobalActor var projectedValue: Wrapped {
    get {  }
    set { }
  }
}

@MainActor
@propertyWrapper
public struct WrapperOnMainActor<Wrapped> {
  // Make sure inference of @MainActor on wrappedValue doesn't crash.
  
  // expected-note@+1 {{mutation of this property is only permitted within the actor}}
  public var wrappedValue: Wrapped

  public var accessCount: Int

  nonisolated public init(wrappedValue: Wrapped) {
    // expected-warning@+1 {{main actor-isolated property 'wrappedValue' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.wrappedValue = wrappedValue
  }
}

@propertyWrapper
public struct WrapperOnMainActorNonSendable<Wrapped> {
  // expected-note@+1 {{mutation of this property is only permitted within the actor}}
  @MainActor public var wrappedValue: Wrapped

  public init(wrappedValue: Wrapped) {
    // expected-warning@+1 {{main actor-isolated property 'wrappedValue' can not be mutated from a nonisolated context; this is an error in the Swift 6 language mode}}
    self.wrappedValue = wrappedValue
  }
}

@propertyWrapper
public struct WrapperOnMainActorSendable<Wrapped: Sendable> {
  @MainActor public var wrappedValue: Wrapped

  public init(wrappedValue: Wrapped) {
    self.wrappedValue = wrappedValue
  }
}

@propertyWrapper
actor WrapperActor<Wrapped: Sendable> {
  var storage: Wrapped

  init(wrappedValue: Wrapped) {
    storage = wrappedValue
  }

  nonisolated var wrappedValue: Wrapped {
    get { }
    set { }
  }

  nonisolated var projectedValue: Wrapped {
    get { }
    set { }
  }
}

struct HasWrapperOnActor {
  @WrapperOnActor var synced: Int = 0
  // expected-note@-1 2{{property declared here}}

  // expected-note@+1 2{{to make instance method 'testErrors()'}}
  func testErrors() {
    _ = synced // expected-error{{main actor-isolated property 'synced' can not be referenced from a nonisolated context}}
    _ = $synced // expected-error{{global actor 'SomeGlobalActor'-isolated property '$synced' can not be referenced from a nonisolated context}}
    _ = _synced // okay
  }

  @MainActor mutating func testOnMain() {
    _ = synced
    synced = 17
  }

  @WrapperActor var actorSynced: Int = 0 // expected-warning{{'nonisolated' is not supported on properties with property wrappers}}

  func testActorSynced() {
    _ = actorSynced
    _ = $actorSynced
    _ = _actorSynced
  }
}


@propertyWrapper
actor WrapperActorBad1<Wrapped> {
  var storage: Wrapped

  init(wrappedValue: Wrapped) {
    storage = wrappedValue
  }

  var wrappedValue: Wrapped { // expected-error{{'wrappedValue' property in property wrapper type 'WrapperActorBad1' cannot be isolated to the actor instance; consider 'nonisolated'}}}}
    get { storage }
    set { storage = newValue }
  }
}

@propertyWrapper
actor WrapperActorBad2<Wrapped: Sendable> {
  var storage: Wrapped

  init(wrappedValue: Wrapped) {
    storage = wrappedValue
  }

  nonisolated var wrappedValue: Wrapped {
    get { }
    set { }
  }

  var projectedValue: Wrapped { // expected-error{{'projectedValue' property in property wrapper type 'WrapperActorBad2' cannot be isolated to the actor instance; consider 'nonisolated'}}
    get {  }
    set {  }
  }
}

@propertyWrapper
struct WrapperWithMainActorDefaultInit {
  var wrappedValue: Int { fatalError() }

  @MainActor init() {} // expected-note {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  // expected-minimal-targeted-note@-1 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
}

actor ActorWithWrapper {
  @WrapperOnActor var synced: Int = 0
  // expected-note@-1 3{{property declared here}}
  @WrapperWithMainActorDefaultInit var property: Int // expected-minimal-targeted-error {{call to main actor-isolated initializer 'init()' in a synchronous actor-isolated context}}
  // expected-complete-tns-warning@-1 {{main actor-isolated default value in a actor-isolated context; this is an error in the Swift 6 language mode}}
  func f() {
    _ = synced // expected-error{{main actor-isolated property 'synced' can not be referenced on a different actor instance}}
    _ = $synced // expected-error{{global actor 'SomeGlobalActor'-isolated property '$synced' can not be referenced on a different actor instance}}
    _ = _synced // expected-error{{global actor 'OtherGlobalActor'-isolated property '_synced' can not be referenced on a different actor instance}}

    @WrapperWithMainActorDefaultInit var value: Int // expected-error {{call to main actor-isolated initializer 'init()' in a synchronous actor-isolated context}}
  }
}

@propertyWrapper
struct WrapperOnSomeGlobalActor<Wrapped: Sendable> {
  private var stored: Wrapped

  nonisolated init(wrappedValue: Wrapped) {
    stored = wrappedValue
  }

  @SomeGlobalActor var wrappedValue: Wrapped {
    get { stored }
    set { stored = newValue }
  }
}

struct InferredFromPropertyWrapper {
  @WrapperOnSomeGlobalActor var value = 17

  func test() -> Int { // expected-note{{calls to instance method 'test()' from outside of its actor context are implicitly asynchronous}}
    value
  }
}

func testInferredFromWrapper(x: InferredFromPropertyWrapper) { // expected-note{{add '@SomeGlobalActor' to make global function 'testInferredFromWrapper(x:)' part of global actor 'SomeGlobalActor'}}
  _ = x.test() // expected-error{{call to global actor 'SomeGlobalActor'-isolated instance method 'test()' in a synchronous nonisolated context}}
}

@propertyWrapper 
struct SimplePropertyWrapper {
  var wrappedValue: Int { .zero }
  var projectedValue: Int { .max }
}

@MainActor
class WrappedContainsNonisolatedAttr {
  @SimplePropertyWrapper nonisolated var value 
  // expected-error@-1 {{'nonisolated' is not supported on properties with property wrappers}}
  // expected-note@-2 {{property declared here}}

  nonisolated func test() {
    _ = value
    _ = $value // expected-error {{main actor-isolated property '$value' can not be referenced from a nonisolated context}}
  }
}


// ----------------------------------------------------------------------
// Unsafe global actors
// ----------------------------------------------------------------------
protocol UGA {
  @preconcurrency @SomeGlobalActor func req() // expected-note{{calls to instance method 'req()' from outside of its actor context are implicitly asynchronous}}
}

struct StructUGA1: UGA {
  @SomeGlobalActor func req() { }
}

struct StructUGA2: UGA {
  nonisolated func req() { }
}

@SomeGlobalActor
struct StructUGA3: UGA {
  func req() {
    sibling()
  }
}

@GenericGlobalActor<String>
func testUGA<T: UGA>(_ value: T) {
  value.req() // expected-warning{{call to global actor 'SomeGlobalActor'-isolated instance method 'req()' in a synchronous global actor 'GenericGlobalActor<String>'-isolated context}}
}

class UGAClass {
  @preconcurrency @SomeGlobalActor func method() { }
}

class UGASubclass1: UGAClass {
  @SomeGlobalActor override func method() { }
}

class UGASubclass2: UGAClass {
  override func method() { }
}

@propertyWrapper
@preconcurrency @OtherGlobalActor
struct WrapperOnUnsafeActor<Wrapped> {
  private var stored: Wrapped

  init(wrappedValue: Wrapped) {
    stored = wrappedValue
  }

  @preconcurrency @MainActor var wrappedValue: Wrapped {
    get { }
    set { }
  }

  @preconcurrency @SomeGlobalActor var projectedValue: Wrapped {
    get { }
    set { }
  }
}

// HasWrapperOnUnsafeActor does not have an inferred global actor attribute,
// because synced and $synced have different global actors.
struct HasWrapperOnUnsafeActor {
// expected-complete-tns-warning@-1 {{memberwise initializer for 'HasWrapperOnUnsafeActor' cannot be both nonisolated and global actor 'OtherGlobalActor'-isolated; this is an error in the Swift 6 language mode}}
// expected-complete-tns-warning@-2 {{default initializer for 'HasWrapperOnUnsafeActor' cannot be both nonisolated and global actor 'OtherGlobalActor'-isolated; this is an error in the Swift 6 language mode}}

  @WrapperOnUnsafeActor var synced: Int = 0 // expected-complete-tns-note 2 {{initializer for property '_synced' is global actor 'OtherGlobalActor'-isolated}}
  // expected-note @-1 2{{property declared here}}
  // expected-complete-tns-note @-2 2{{property declared here}}

  func testUnsafeOkay() {
    // expected-complete-tns-note @-1 {{add '@SomeGlobalActor' to make instance method 'testUnsafeOkay()' part of global actor 'SomeGlobalActor'}}
    // expected-complete-tns-note @-2 {{add '@MainActor' to make instance method 'testUnsafeOkay()' part of global actor 'MainActor'}}
    _ = synced // expected-complete-tns-warning {{main actor-isolated property 'synced' can not be referenced from a nonisolated context}}
    _ = $synced // expected-complete-tns-warning {{global actor 'SomeGlobalActor'-isolated property '$synced' can not be referenced from a nonisolated context}}
    _ = _synced // okay
  }

  nonisolated func testErrors() {
    _ = synced // expected-warning{{main actor-isolated property 'synced' can not be referenced from a nonisolated context}}
    _ = $synced // expected-warning{{global actor 'SomeGlobalActor'-isolated property '$synced' can not be referenced from a nonisolated context}}
    _ = _synced // okay
  }

  @MainActor mutating func testOnMain() {
    _ = synced
    synced = 17
  }
}

nonisolated func createHasWrapperOnUnsafeActor() {
  _ = HasWrapperOnUnsafeActor()
}

// ----------------------------------------------------------------------
// Nonisolated closures
// ----------------------------------------------------------------------
@SomeGlobalActor func getGlobal7() -> Int { 7 }
func acceptClosure<T>(_: () -> T) { }

@SomeGlobalActor func someGlobalActorFunc() async {
  acceptClosure { getGlobal7() } // okay
}

// ----------------------------------------------------------------------
// Main actor that predates concurrency
// ----------------------------------------------------------------------
@preconcurrency func takesUnsafeMainActor(fn: @MainActor () -> Void) { }

@MainActor func onlyOnMainActor() { }

func useUnsafeMainActor() {
  takesUnsafeMainActor {
    onlyOnMainActor() // okay due to parameter attribute
  }
}

// ----------------------------------------------------------------------
// @IBAction implies @MainActor(unsafe)
// ----------------------------------------------------------------------
class SomeWidgetThing {
  @IBAction func onTouch(_ object: AnyObject) {
    onlyOnMainActor() // okay
  }
}

// ----------------------------------------------------------------------
// @_inheritActorContext
// ----------------------------------------------------------------------
func acceptAsyncSendableClosure<T>(_: @Sendable () async -> T) { }
func acceptAsyncSendableClosureInheriting<T>(@_inheritActorContext _: @Sendable () async -> T) { }

@MainActor func testCallFromMainActor() {
  acceptAsyncSendableClosure {
    onlyOnMainActor() // expected-error{{main actor-isolated global function 'onlyOnMainActor()' cannot be called from outside of the actor}} {{5-5=await }}
  }

  acceptAsyncSendableClosure {
    await onlyOnMainActor() // okay
  }

  acceptAsyncSendableClosureInheriting {
    onlyOnMainActor() // okay
  }

  acceptAsyncSendableClosureInheriting {
    await onlyOnMainActor() // expected-warning{{no 'async' operations occur within 'await' expression}}
  }
}


// defer bodies inherit global actor-ness
@MainActor
var statefulThingy: Bool = false // expected-minimal-targeted-note {{var declared here}}
// expected-complete-tns-error @-1 {{top-level code variables cannot have a global actor}}

@MainActor
func useFooInADefer() -> String { // expected-minimal-targeted-note {{calls to global function 'useFooInADefer()' from outside of its actor context are implicitly asynchronous}}
  defer {
    statefulThingy = true
  }

  return "hello"
}

// ----------------------------------------------------------------------
// Dynamic replacement
// ----------------------------------------------------------------------
@_dynamicReplacement(for: dynamicOnMainActor)
func replacesDynamicOnMainActor() {
  onlyOnMainActor()
}

// ----------------------------------------------------------------------
// Global-actor isolation of stored property initializer expressions
// ----------------------------------------------------------------------

// expected-complete-tns-warning@+1 {{default initializer for 'Cutter' cannot be both nonisolated and main actor-isolated; this is an error in the Swift 6 language mode}}
class Cutter {
  // expected-complete-tns-note@+1 {{initializer for property 'x' is main actor-isolated}}
  @MainActor var x = useFooInADefer()
  @MainActor var y = { () -> Bool in
      var z = statefulThingy
      return z
    }()
}

@SomeGlobalActor
class Butter {
  var a = useFooInADefer() // expected-minimal-targeted-error {{call to main actor-isolated global function 'useFooInADefer()' in a synchronous global actor 'SomeGlobalActor'-isolated context}}
  // expected-complete-tns-warning@-1 {{main actor-isolated default value in a global actor 'SomeGlobalActor'-isolated context; this is an error in the Swift 6 language mode}}

  nonisolated let b = statefulThingy // expected-minimal-targeted-error {{main actor-isolated var 'statefulThingy' can not be referenced from a nonisolated context}}
  // expected-complete-tns-warning@-1 {{main actor-isolated default value in a nonisolated context; this is an error in the Swift 6 language mode}}

  var c: Int = {
    return getGlobal7()
  }()

  lazy var d: Int = getGlobal7()

  static var e: Int = getGlobal7()
}
