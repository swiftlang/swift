// RUN: %target-typecheck-verify-swift -enable-experimental-feature TypeWrappers

@typeWrapper
struct ConcreteTypeWrapper { // expected-error {{type wrapper has to declare a single generic parameter for underlying storage type}}
  init(memberwise: Int) {}
}

@typeWrapper
struct EmptyTypeWrapper<S> { // expected-error {{type wrapper type 'EmptyTypeWrapper' does not contain a required initializer - init(memberwise:)}}
}

@typeWrapper
struct NoMemberwiseInit<S> {
  // expected-error@-1 {{type wrapper type 'NoMemberwiseInit' does not contain a required initializer - init(memberwise:)}}

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct FailableInit<S> {
  init?(memberwise: S) { // expected-error {{type wrapper initializer 'init(memberwise:)' cannot be failable}}
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

// Okay because there is a valid `init(memberwise:)` overload.
@typeWrapper
struct FailableAndValidInit<S> {
  init(memberwise: S) {
  }

  init?(memberwise: S) where S == Int {
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
public struct InaccessibleInit<S> {
  fileprivate init(memberwise: S) {
    // expected-error@-1 {{fileprivate initializer 'init(memberwise:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
  }

  private init?(memberwise: S) where S: AnyObject {
    // expected-error@-1 {{private initializer 'init(memberwise:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
    // expected-error@-2 {{type wrapper initializer 'init(memberwise:)' cannot be failable}}
  }

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct NoSubscripts<S> {
  // expected-error@-1 {{type wrapper type 'NoSubscripts' does not contain a required subscript - subscript(storedKeyPath:)}}
  init(memberwise: S) {}
}

@typeWrapper
struct InaccessibleOrInvalidSubscripts<S> {
  init(memberwise: S) {}

  fileprivate subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    // expected-error@-1 {{fileprivate subscript 'subscript(storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  private subscript<V>(storageKeyPath path: KeyPath<S, V>) -> [V] {
    // expected-error@-1 {{private subscript 'subscript(storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  subscript(storageKeyPath path: Int) -> Bool { // expected-error {{type wrapper subscript expects a key path parameter type (got: 'Int')}}
    get { true }
  }
}

@typeWrapper
struct NoopWrapper<S> {
  init(memberwise: S) {}

  subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
    set { }
  }
}

@NoopWrapper
struct A {
  var a: String
  var b: Int
}

@NoopWrapper
class GenericA<K: Hashable, V> {
  var data: [K: V]
}

@NoopWrapper // expected-error {{type wrapper attribute 'NoopWrapper' can only be applied to a class, struct}}
protocol P {
}

@NoopWrapper // expected-error {{type wrapper attribute 'NoopWrapper' can only be applied to a class, struct}}
enum E {
  var x: Int { get { 42 } }
}

func testWrappedTypeAccessChecking() {
  let a = A(a: "", b: 42) // synthesized init
  let b = GenericA(data: ["ultimate question": 42]) // generic synthesized init

  _ = a.a // Ok
  _ = b.data // Ok
}

struct Parent {
  @typeWrapper
  struct Wrapper<S> {
    init(memberwise: S) {}

    subscript<V>(storageKeyPath path: KeyPath<S, V>) -> V {
      get { fatalError() }
      set { }
    }
  }
}

func testLocalWithNestedWrapper() {
  @Parent.Wrapper
  class WithNestedWrapper<T> {
    var test: [T]

    var computed: String {
      get { "" }
    }
  }

  let t = WithNestedWrapper(test: [1, 2]) // synthesized init
  _ = t.test // Ok
  _ = t.computed // Ok
}

func testTypeWrapperWithDefaults() {
  @NoopWrapper
  struct A {
    var question: String = "Ultimate Question"
    var answer: Int = 42
  }

  let a = A()
  _ = a.question
  _ = a.answer

  _ = A(question: "")
  _ = A(answer: 0)
}

/// Properties with property wrappers

@propertyWrapper
struct Wrapper<Value> {
  public var value: Value

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  var projectedValue: Self { return self }

  var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

@propertyWrapper
struct WrapperWithoutInit<Value> {
  public var value: Value

  var projectedValue: Self { return self }

  var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

@propertyWrapper
struct WrapperWithoutProjection<Value> {
  public var value: Value

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }

  var wrappedValue: Value {
    get {
      self.value
    }
    set {
      self.value = newValue
    }
  }
}

func propertyWrapperTests() {
  @NoopWrapper
  struct WrapperTest {
    @Wrapper var test: Int
  }

  _ = WrapperTest(test: 42) // Ok

  @NoopWrapper
  struct DefaultedWrapperTest {
    @Wrapper var test: Int = 42
  }

  _ = DefaultedWrapperTest() // Ok
  _ = DefaultedWrapperTest(test: 42) // Ok

  @NoopWrapper
  struct NoInitTest {
    @WrapperWithoutInit var test: Int
  }

  _ = NoInitTest(test: WrapperWithoutInit(value: 42)) // Ok

  @NoopWrapper
  struct DefaultedNoInitTest {
    @WrapperWithoutInit(value: 42) var test: Int
  }

  _ = DefaultedNoInitTest() // Ok
  _ = DefaultedNoInitTest(test: WrapperWithoutInit(value: 0)) // Ok

  @NoopWrapper
  struct NoProjection {
    @WrapperWithoutProjection var test: Int // expected-note {{'test' declared here}}
  }

  let noProj = NoProjection(test: 42) // Ok
  _ = noProj.test // Ok
  _ = noProj.$test // expected-error {{value of type 'NoProjection' has no member '$test'}}

  @NoopWrapper
  struct NoInitComposition1 {
    @Wrapper @WrapperWithoutInit var test: Int
  }

  _ = NoInitComposition1(test: Wrapper(wrappedValue: WrapperWithoutInit(value: 42))) // Ok

  @NoopWrapper
  struct NoInitComposition2 {
    @WrapperWithoutInit @Wrapper var test: Int
  }

  _ = NoInitComposition2(test: WrapperWithoutInit(value: Wrapper(wrappedValue: 42))) // Ok

  @NoopWrapper
  struct NoInitCompositionWithDefault {
    @Wrapper(wrappedValue: WrapperWithoutInit(value: 42)) @WrapperWithoutInit var test: Int
  }

  _ = NoInitCompositionWithDefault() // Ok
  _ = NoInitCompositionWithDefault(test: Wrapper(wrappedValue: WrapperWithoutInit(value: 0))) // Ok

  @NoopWrapper
  struct Complex1 {
    @Wrapper var a: Int = 42
    var b: String = ""
  }

  _ = Complex1()
  _ = Complex1(b: "hello") // Ok
  _ = Complex1(a: 0) // Ok
  _ = Complex1(a: 0, b: "") // Ok

  @NoopWrapper
  struct Complex2 {
    @Wrapper var a: Int = 42
    @WrapperWithoutInit @Wrapper var b: String
  }

  _ = Complex2(b: WrapperWithoutInit(value: Wrapper(wrappedValue: "hello"))) // Ok
  _ = Complex2(a: 0, b: WrapperWithoutInit(value: Wrapper(wrappedValue: "hello"))) // Ok
  _ = Complex2(b: WrapperWithoutInit(value: Wrapper(wrappedValue: "wrong")), a: 0) // expected-error {{argument 'a' must precede argument 'b'}}

  @NoopWrapper
  struct Complex3 {
    @Wrapper var a: Int = 42
    var b: String = ""
    @WrapperWithoutProjection var c: [Int] = []
  }

  _ = Complex3()
  _ = Complex3(b: "hello") // Ok
  _ = Complex3(c: [1, 2, 3]) // Ok
  _ = Complex3(a: 0) // Ok
  _ = Complex3(a: 0, b: "") // Ok
  _ = Complex3(a: 0, b: "", c: [1, 2, 3]) // Ok

  @NoopWrapper
  struct Invalid {
    @Wrapper(wrappedValue: "") var a: Int
    // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'Int'}}

    @Wrapper var b: Int = ""
    // expected-error@-1 {{cannot convert value of type 'String' to specified type 'Int'}}

    @Wrapper(wrappedValue: 0) var c: Int = 1
    // expected-error@-1 {{extra argument 'wrappedValue' in call}}

    @Wrapper(other: "") var d: Float
    // expected-error@-1 {{incorrect argument label in call (have 'other:', expected 'wrappedValue:')}}
    // expected-error@-2 {{cannot convert value of type 'String' to expected argument type 'Float'}}
  }
}

func testDeclarationsWithUnmanagedProperties() {
  @NoopWrapper
  struct WithLet { // expected-note {{'init(name:age:)' declared here}}
    let name: String
    var age: Int
  }
  _ = WithLet(age: 0) // expected-error {{missing argument for parameter 'name' in call}}
  _ = WithLet(name: "", age: 0) // Ok

  @NoopWrapper
  struct WithDefaultedLet {
    let name: String = "Arthur Dent"
    var age: Int
  }

  _ = WithDefaultedLet(age: 32) // Ok
  _ = WithDefaultedLet(name: "", age: 0) // expected-error {{extra argument 'name' in call}}

  @NoopWrapper
  struct WithLazy {
    lazy var name: String = {
      "Arthur Dent"
    }()

    var age: Int = 30
  }

  _ = WithLazy() // Ok
  _ = WithLazy(name: "") // expected-error {{extra argument 'name' in call}}
  _ = WithLazy(name: "", age: 0) // expected-error {{extra argument 'name' in call}}
  _ = WithLazy(age: 0) // Ok

  @NoopWrapper
  struct OnlyLazyLetAndComputed {
    let name: String
    lazy var age: Int = {
      30
    }()
    var planet: String {
      get { "Earth" }
    }
  }

  _ = OnlyLazyLetAndComputed(name: "Arthur Dent") // Ok
}
