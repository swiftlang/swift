// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature TypeWrappers

// REQUIRES: asserts

@typeWrapper
struct ConcreteTypeWrapper { // expected-error {{type wrapper has to declare two generic parameters: wrapped and storage types}}
  init(storage: Int) {}
}

@typeWrapper
struct EmptyTypeWrapper<W, S> { // expected-error {{type wrapper type 'EmptyTypeWrapper' does not contain a required initializer - init(for:storage:)}}
}

@typeWrapper
struct NoMemberwiseInit<W, S> {
  // expected-error@-1 {{type wrapper type 'NoMemberwiseInit' does not contain a required initializer - init(for:storage:)}}

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct FailableInit<W, S> {
  init?(for: W.Type, storage: S) { // expected-error {{type wrapper initializer 'init(for:storage:)' cannot be failable}}
  }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

// Okay because there is a valid `init(for:storage:)` overload.
@typeWrapper
struct FailableAndValidInit<W, S> {
  // expected-error@-1 {{type wrapper type 'FailableAndValidInit' does not contain a required writable subscript}}
  // expected-note@-2 {{do you want to add a stub?}} {{36-36=\nsubscript<Value>(propertyKeyPath propPath: KeyPath<<#WrappedType#>, Value>, storageKeyPath storagePath: WritableKeyPath<<#Base#>, Value>) -> Value { get { <#code#> \} set { <#code#> \} \}}}

  init(for: W.Type, storage: S) {
  }

  init?(for: W.Type, storage: S) where S == Int {
  }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
public struct InaccessibleInit<W, S> {
  fileprivate init(for: W.Type, storage: S) {
    // expected-error@-1 {{fileprivate initializer 'init(for:storage:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
  }

  private init?(for: W.Type, storage: S) where S: AnyObject {
    // expected-error@-1 {{private initializer 'init(for:storage:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleInit' (which is public)}}
    // expected-error@-2 {{type wrapper initializer 'init(for:storage:)' cannot be failable}}
  }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }
}

@typeWrapper
struct NoSubscripts<W, S> {
  // expected-error@-1 {{type wrapper type 'NoSubscripts' does not contain a required subscript - subscript(propertyKeyPath:storageKeyPath:)}}
  init(for: W.Type, storage: S) {}
}

@typeWrapper
struct InaccessibleOrInvalidSubscripts<W, S> {
  init(for: W.Type, storage: S) {}

  fileprivate subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    // expected-error@-1 {{fileprivate subscript 'subscript(propertyKeyPath:storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  private subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> [V] {
    // expected-error@-1 {{private subscript 'subscript(propertyKeyPath:storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  private subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> [V] {
    // expected-error@-1 {{private subscript 'subscript(propertyKeyPath:storageKeyPath:)' cannot have more restrictive access than its enclosing type wrapper type 'InaccessibleOrInvalidSubscripts' (which is internal)}}
    get { fatalError() }
  }

  subscript(propertyKeyPath _: KeyPath<W, Bool>, storageKeyPath path: Int) -> Bool { // expected-error {{type wrapper subscript parameter 'storageKeyPath' expects a key path (got: 'Int')}}
    get { true }
  }

  subscript(propertyKeyPath _: Int.Type, storageKeyPath path: KeyPath<S, Bool>) -> Bool {
    // expected-error@-1 {{type wrapper subscript parameter 'propertyKeyPath' expects a key path (got: 'Int.Type')}}
    get { true }
  }
}

@typeWrapper
struct OverloadedCtorWrapper<W, S> {
  init(for: W.Type, storage: S) {} // expected-error {{cannot overload type wrapper initializer 'init(for:storage:)'}}
  init(for: W.Type, storage: Int) {}

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> [V] { get { fatalError() } }
  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> [V] {
    get { fatalError() }
    set { }
  }
}

do {
  @typeWrapper
  struct InvalidInit1<W, S> {
    init(for: Int, storage: S) {} // expected-error {{first parameter of type wrapper initializer should be wrapped type - 'W.Type'}}
  }

  @typeWrapper
  struct InvalidInit2<W, S> {
    init(for: W, storage: S) {} // expected-error {{first parameter of type wrapper initializer should be wrapped type - 'W.Type'}}
  }

  @typeWrapper
  struct InvalidInit3<W, S> {
    init(for: Int.Type, storage: S) {} // expected-error {{first parameter of type wrapper initializer should be wrapped type - 'W.Type'}}
  }

  @typeWrapper
  struct InvalidInit4<W, S> {
    init(for: W.Type, storage: Int) {} // expected-error {{second parameter of type wrapper initializer should be storage type - 'S'}}
  }

  @typeWrapper
  struct InvalidInit5<W, S> {
    init(for: W.Type, storage: UnknownType) {} // expected-error {{second parameter of type wrapper initializer should be storage type - 'S'}}
    // expected-error@-1 {{cannot find type 'UnknownType' in scope}}
  }

  @typeWrapper
  struct InvalidInit6<W, S> {
    init(for: UnknownType.Type, storage: S) {} // expected-error {{first parameter of type wrapper initializer should be wrapped type - 'W.Type'}}
    // expected-error@-1 {{cannot find type 'UnknownType' in scope}}
  }
}

@typeWrapper
struct NoopWrapper<W, S> {
  // expected-note@-1 {{arguments to generic parameter 'W' ('Test1' and 'Test2') are expected to be equal}}
  // expected-note@-2 {{arguments to generic parameter 'S' ('Test1.$Storage' and 'Test2.$Storage') are expected to be equal}}

  init(for: W.Type, storage: S) {}

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
    get { fatalError() }
  }

  subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
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

@NoopWrapper
protocol P { // Ok
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
  struct Wrapper<W, S> {
    init(for: W.Type, storage: S) {}

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
      get { fatalError() }
    }

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
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
  _ = WithDefaultedLet(name: "", age: 0) // Ok

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

/* (!) FIXME: It's not possible to form a key path to an actor isolated property (rdar://84445219)
 *
func testActors() async {
  @NoopWrapper
  actor Person {
    var name: String
    note@-1 {{mutation of this property is only permitted within the actor}}
    var age: Int
    ote@-1 {{mutation of this property is only permitted within the actor}}
  }

  let person = Person(name: "Arthur Dent", age: 30)

  _ = await person.name
  _ = await person.age

  person.name = "NoName"
  error@-1 {{actor-isolated property 'name' can not be mutated from a non-isolated context}}
  person.age = 0
  error@-1 {{actor-isolated property 'age' can not be mutated from a non-isolated context}}
}
*/

func testIgnoredAttr() {
  @NoopWrapper
  struct X {
    @typeWrapperIgnored static var staticVar: Int = 0 // expected-error {{@typeWrapperIgnored must not be used on static properties}}

    @typeWrapperIgnored lazy var lazyVar: Int = { // expected-error {{@typeWrapperIgnored must not be used on lazy properties}}
      42
    }()

    @typeWrapperIgnored var x: Int // Ok

    var y: Int {
      @typeWrapperIgnored get { // expected-error {{@typeWrapperIgnored may only be used on 'var' declarations}}
        42
      }

      @typeWrapperIgnored set { // expected-error {{@typeWrapperIgnored may only be used on 'var' declarations}}
      }
    }

    @typeWrapperIgnored var computed: Int { // expected-error {{@typeWrapperIgnored must not be used on computed properties}}
      get { 42 }
      set {}
    }

    @typeWrapperIgnored let z: Int = 0 // expected-error {{@typeWrapperIgnored may only be used on 'var' declarations}}

    func test_param(@typeWrapperIgnored x: Int) {} // expected-error {{@typeWrapperIgnored may only be used on 'var' declarations}}

    func test_local() {
      @typeWrapperIgnored var x: Int = 42 // expected-error {{@typeWrapperIgnored must not be used on local properties}}
      // expected-warning@-1 {{variable 'x' was never used; consider replacing with '_' or removing it}}

      @typeWrapperIgnored let y: String // expected-error {{@typeWrapperIgnored must not be used on local properties}}
      // expected-warning@-1 {{immutable value 'y' was never used; consider replacing with '_' or removing it}}
    }
  }
}

func testMissingReadOnlyAndWritableSubscriptsAreDiagnosed() {
  @typeWrapper
  struct MissingReadOnly<W, S> {
    // expected-error@-1 {{type wrapper type 'MissingReadOnly' does not contain a required ready-only subscript}}
    // expected-note@-2 {{do you want to add a stub?}} {{33-33=\nsubscript<Value>(propertyKeyPath propPath: KeyPath<<#WrappedType#>, Value>, storageKeyPath storagePath: KeyPath<<#Base#>, Value>) -> Value { get { <#code#> \} \}}}

    init(for: W.Type, storage: S) {}

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
      get { fatalError() }
      set { }
    }
  }

  @typeWrapper
  struct MissingWritable<W, S> {
    // expected-error@-1 {{type wrapper type 'MissingWritable' does not contain a required writable subscript}}
    // expected-note@-2 {{do you want to add a stub?}} {{33-33=\nsubscript<Value>(propertyKeyPath propPath: KeyPath<<#WrappedType#>, Value>, storageKeyPath storagePath: WritableKeyPath<<#Base#>, Value>) -> Value { get { <#code#> \} set { <#code#> \} \}}}

    init(for: W.Type, storage: S) {}

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
      get { fatalError() }
    }
  }
}

func testIncorrectUsesOfImmutableProperties() {
  class X<T> {
    var storage: [T]

    init(storage: [T]) {
      self.storage = storage
    }
  }

  @NoopWrapper
  struct Test<T> {
    let x: T? // expected-note {{change 'let' to 'var' to make it mutable}}

    init(x: T) {
      self.x = x
    }
  }

  let test = Test(x: X(storage: [1, 2, 3]))
  test.x = X(storage: [0]) // expected-error {{cannot assign to property: 'x' is a 'let' constant}}
  test.x?.storage.append(0) // Ok
}

func testWrappedSelfInReferenceOnlySubscript() {
  @typeWrapper
  struct WrappedSelfTests<W, S> {
    init(for: W.Type, storage: S) {}

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V {
      get { fatalError() }
      set { }
    }

    subscript<V>(propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V {
      get { fatalError() }
      set { }
    }

    subscript<V>(wrappedSelf w: W, propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: WritableKeyPath<S, V>) -> V { // Ok
      get { fatalError() }
      set { }
    }

    subscript<V>(wrappedSelf w: W, propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V { // Ok
      get { fatalError() }
    }

    subscript<V>(wrappedSelf w: Int, propertyKeyPath _: KeyPath<W, V>, storageKeyPath path: KeyPath<S, V>) -> V { // expected-error {{type wrapper subscript parameter 'wrappedSelf' expects type 'W' (got: 'Int')}}
      get { fatalError() }
    }

    subscript<V>(wrappedSelf w: Int, propertyKeyPath _: String, storageKeyPath path: [Int]) -> V {
      // expected-error@-1 {{type wrapper subscript parameter 'wrappedSelf' expects type 'W' (got: 'Int')}}
      // expected-error@-2 {{subscript parameter 'propertyKeyPath' expects a key path (got: 'String')}}
      // expected-error@-3 {{type wrapper subscript parameter 'storageKeyPath' expects a key path (got: '[Int]'}}
      get { fatalError() }
    }
  }
}

// rdar://99884355 - Missing argument for parameter 'name'
do {
  struct Use {
    var product = Product(name: "<no name>")
  }

  @NoopWrapper
  struct Product {
    var name: String
  }
}

do {
  @NoopWrapper
  struct Test1 {
    var a: Int
    var b: [String]
  }

  let wrapper = NoopWrapper(for: Test1.self, storage: Test1.$Storage(a: 42, b: [""]))
  _ = Test1(storageWrapper: wrapper) // Ok

  @NoopWrapper
  struct Test2 {
  }

  _ = Test2(storageWrapper: NoopWrapper(for: Test2.self, storage: Test2.$Storage())) // Ok
  _ = Test2(storageWrapper: wrapper)
  // expected-error@-1 {{cannot convert value of type 'NoopWrapper<Test1, Test1.$Storage>' to expected argument type 'NoopWrapper<Test2, Test2.$Storage>'}}

  @NoopWrapper
  struct Test3 { // expected-note {{'init(a:b:)' declared here}}
    var a: Int
    @typeWrapperIgnored var b: String
  }

  // @typeWrapperIgnored suppresses `storageWrapper:` initializer
  _ = Test3(storageWrapper: NoopWrapper(for: Test3.self, storage: Test3.$Storage(a: 42)))
  // expected-error@-1 {{missing arguments for parameters 'a', 'b' in call}}
  // expected-error@-2 {{extra argument 'storageWrapper' in call}}
}

func test_multiple_wrapper_attributes() {
  @Parent.Wrapper @NoopWrapper
  // expected-note@-1 {{'Wrapper' declared here}}
  // expected-note@-2 {{'NoopWrapper' declared here}}
  struct Test1 {} // expected-error {{struct 'Test1' cannot use more than one type wrapper}}

  @Parent.Wrapper @NoopWrapper
  // expected-note@-1 {{'Wrapper' declared here}}
  // expected-note@-2 {{'NoopWrapper' declared here}}
  class Test2 {} // expected-error {{class 'Test2' cannot use more than one type wrapper}}
}

@NoopWrapper
protocol WrappedProto {
}

do {
  // @NoopWrapper is inferred from `WrappedProto`
  struct InferenceFromProto : WrappedProto { // Ok
    var x: Int

    func test() {
      _ = $storage // Ok (to make sure that NoopWrapper is indeed inferred)
    }
  }

  @Parent.Wrapper // expected-note {{'Wrapper' declared here}}
  struct ClashBetweenDirectAndInferred : WrappedProto {
    // expected-error@-1 {{struct 'ClashBetweenDirectAndInferred' cannot use more than one type wrapper}}
    // expected-note@-2 {{type wrapper 'NoopWrapper' inferred from protocol 'WrappedProto'}}
    // expected-error@-3 {{type 'ClashBetweenDirectAndInferred' does not conform to protocol 'WrappedProto'}}
  }

  @NoopWrapper
  final class NoClashDueToSameWrapper : WrappedProto { // Ok
    var v: [Int?]

    func test() {
      let storage: NoopWrapper<NoClashDueToSameWrapper, $Storage> = $storage
      _ = storage[propertyKeyPath: \.v, storageKeyPath: \$Storage.v]
    }
  }


}
