// RUN: %target-typecheck-verify-swift

func entity(_: Int) -> Int {
  0
}

struct Test {
  func test(_ v: Int) -> Int { v }
  func test(_ v: Int?) -> Int? { v }
}

func test_ternary_literal(v: Test) -> Int? {
  true ? v.test(0) : nil // Ok
}

func test_ternary(v: Test) -> Int? {
  // Because calls had favored types set if they were resolved during constraint generation.
  true ? v.test(entity(0)) : nil // Ok
}

do {
  struct TestFloat {
    func test(_ v: Float) -> Float { v } // expected-note {{found this candidate}}
    func test(_ v: Float?) -> Float? { v } // expected-note {{found this candidate}}
  }

  func test_ternary_non_default_literal(v: TestFloat) -> Float? {
    true ? v.test(1.0) : nil // expected-error {{ambiguous use of 'test'}}
  }
}

do {
  struct Test {
    init(a: Int, b: Int = 0) throws {}
    init?(a: Int?) {}
  }

  func test(v: Int) -> Test? {
    return Test(a: v) // Ok
  }
}

// error: initializer for conditional binding must have Optional type, not 'S'
do {
  struct S {
    let n: Int

    func test(v: String) -> Int { }
    func test(v: String, flag: Bool = false) -> Int? { }


    func verify(v: String) -> Int? {
      guard let _ = test(v: v) else { // Ok
        return nil
      }
      return 0
    }
  }

  func f(_: String, _ p: Bool = false) -> S? {
    nil
  }

  func f(_ x: String) -> S {
    fatalError()
  }

  func g(_ x: String) -> Int? {
    guard let y = f(x) else {
      return nil
    }
    return y.n
  }
}

// ambiguities related to ~=
protocol _Error: Error {}

extension _Error {
  public static func ~=(lhs: Self, rhs: Self) -> Bool {
    false
  }

  public static func ~=(lhs: Error, rhs: Self) -> Bool {
    false
  }

  public static func ~=(lhs: Self, rhs: Error) -> Bool {
    false
  }
}

enum CustomError {
  case A
}

extension CustomError: _Error {}

func f(e: CustomError) {
  if e ~= CustomError.A {}
}

// Generic overload should be preferred over concrete one because the latter is non-default literal
struct Pattern {}

func ~= (pattern: Pattern, value: String) -> Bool {
  return false
}

extension Pattern: ExpressibleByStringLiteral {
  init(stringLiteral value: String) {}
}

func test_default_tilda(v: String) {
  _ = "hi" ~= v // Ok
}

struct UUID {}

struct LogKey {
  init(base: some CustomStringConvertible, foo: Int = 0) {
  }

  init(base: UUID, foo: Int = 0) {
  }
}

@available(swift 99)
extension LogKey {
  init(base: String?) {
  }

  init(base: UUID?) {
  }
}

func test_that_unavailable_init_is_not_used(x: String?) {
  _ = LogKey(base: x ?? "??")
}

// error: value of optional type 'UID?' must be unwrapped to a value of type 'UID'
struct S: Comparable {
  static func <(lhs: Self, rhs: Self) -> Bool {
    false
  }
}

func max(_ a: S?, _ b: S?) -> S? {
  nil
}

func test_stdlib_max_selection(s: S) -> S {
  let new = max(s, s)
  return new // Ok
}

// error: initializer for conditional binding must have Optional type, not 'UnsafeMutablePointer<Double>'
do {
  struct TestPointerConversions {
    var p: UnsafeMutableRawPointer { get { fatalError() } }

    func f(_ p: UnsafeMutableRawPointer) {
      // The old hack (which is now removed) couldn't handle member references, only direct declaration references.
      guard let x = UnsafeMutablePointer<Double>(OpaquePointer(self.p)) else {
        return
      }
      _ = x

      guard let x = UnsafeMutablePointer<Double>(OpaquePointer(p)) else {
        // expected-error@-1 {{initializer for conditional binding must have Optional type, not 'UnsafeMutablePointer<Double>'}}
        return
      }
      _ = x
    }
  }
}

// error: initializer 'init(_:)' requires that 'T' conform to 'BinaryInteger'
do {
  struct Config {
    subscript<T>(_ key: String) -> T? { nil }
    subscript(_ key: String) -> Any? { nil }
  }

  struct S {
	  init(maxQueueDepth: UInt) {}
  }

  func f(config: Config) {
    let maxQueueDepth = config["hi"] ?? 256
    _ = S(maxQueueDepth: UInt(maxQueueDepth))
  }
}

// `tryOptimizeGenericDisjunction` is too aggressive sometimes, make sure that `<T: FloatingPoint>`
// overload is _not_ selected in this case.
do {
  func test<T: FloatingPoint>(_ expression1: @autoclosure () throws -> T, accuracy: T) -> T {}
  func test<T: Numeric>(_ expression1: @autoclosure () throws -> T, accuracy: T) -> T {}

  let result = test(10, accuracy: 1)
  let _: Int = result
}

// swift-distributed-tracing snippet that relies on old hack behavior.
protocol TracerInstant {
}

extension Int: TracerInstant {}

do {
  enum SpanKind {
    case `internal`
  }

  func withSpan<Instant: TracerInstant>(
    _ operationName: String,
    at instant: @autoclosure () -> Instant,
    context: @autoclosure () -> Int = 0,
    ofKind kind: SpanKind = .internal
  ) {}

  func withSpan(
     _ operationName: String,
     context: @autoclosure () -> Int = 0,
     ofKind kind: SpanKind = .internal,
     at instant: @autoclosure () -> some TracerInstant = 42
  ) {}

  withSpan("", at: 0) // Ok
}

protocol ForAssert {
  var isEmpty: Bool { get }
}

extension ForAssert {
  var isEmpty: Bool { false }
}

do {
  func assert(_ condition: @autoclosure () -> Bool, _ message: @autoclosure () -> String = String(), file: StaticString = #file, line: UInt = #line) {}
  func assert(_ condition: Bool, _ message: @autoclosure () -> String, file: StaticString = #file, line: UInt = #line) {}
  func assert(_ condition: Bool, file: StaticString = #fileID, line: UInt = #line) {}

  struct S : ForAssert {
    var isEmpty: Bool { false }
  }

  func test(s: S) {
    assert(s.isEmpty, "") // Ok
  }
}

extension Double {
    public static func * (left: Float, right: Double) -> Double { 0 }
}

func test_non_default_literal_use(arg: Float) {
    let v = arg * 2.0 // shouldn't use `(Float, Double) -> Double` overload
    let _: Float = v // Ok
}

// This should be ambiguous without contextual type but was accepted before during to
// unlabeled unary argument favoring.
func test_variadic_static_member_is_preferred_over_partially_applied_instance_overload() {
  struct Test {
    func fn() {}
    static func fn(_: Test...) {}
  }

  let t: Test
  Test.fn(t) // Ok
}

// Unary unlabeled argument favoring hacks never applied to subscripts

protocol Subscriptable {
}

extension Subscriptable {
  subscript(key: String) -> Any? { nil }
}

struct MyValue {}

extension Dictionary<String, MyValue> : Subscriptable {}

func test_that_unary_argument_hacks_do_not_apply_to_subscripts(dict: [String: MyValue]) {
  let value = dict["hello"]
  let _: MyValue? = value // Ok
}

// Unlabeled unary argument hack was disabled if there were any protocol requirements
// or variadic generic overloads present in the result set (regadless of their viability).
//
// Remove the requirement and variadic overloads and this code would start failing even
// though it shouldn't!

struct Future<T> {
}

protocol DB {
  func get(_: Int, _: Int) -> Future<Int?>
}

extension DB {
  func get(_: Int, _: Int = 42) async throws -> Int? { nil }
  func get(_: Int) -> Future<Int?> { .init() }

  func fetch(_: Int, _: Int = 42) async throws -> Int? { nil }
  func fetch(_: Int) -> Future<Int?> { .init() }
  func fetch<each T>(values: repeat each T) -> Int { 42 }
}

struct TestUnary {
  var db: any DB

  func get(v: Int) async throws {
    guard let _ = try await self.db.get(v) else { // Ok
      return
    }

    guard let _ = try await self.db.fetch(v) else { // Ok
      return
    }
  }
}

// Prevent non-optional overload of `??` to be favored when all initializers are failable.

class A {}
class B {}

protocol P {
  init()
}

extension P {
  init?(v: A) { self.init() }
}

struct V : P {
  init() {}

  @_disfavoredOverload
  init?(v: B?) {}

  // Important to keep this to make sure that disabled constraints
  // are handled properly.
  init<T: Collection>(other: T) where T.Element == Character {}
}

class TestFailableOnly {
  var v: V?

  func test(defaultB: B) {
    guard let _ = self.v ?? V(v: defaultB) else { // OK (no warnings)
      return
    }
  }
}

do {
  @_disfavoredOverload
  func test(over: Int, that: String = "", block: @escaping (Int) throws -> Void) async throws {}
  func test(over: Int, that: String = "", block: @escaping (Int) throws -> Void) throws {} // expected-note {{found this candidate}}
  func test(over: Int, other: String = "", block: @escaping (Int) throws -> Void) throws {} // expected-note {{found this candidate}}

  func performLocal(v: Int, block: @escaping (Int) throws -> Void) async throws {
    try await test(over: v, block: block) // expected-error {{ambiguous use of 'test'}}
  }

  // The hack applied only to `OverloadedDeclRefExpr`s.
  struct MemberTest {
    @_disfavoredOverload
    func test(over: Int, that: String = "", block: @escaping (Int) throws -> Void) async throws {}
    func test(over: Int, that: String = "", block: @escaping (Int) throws -> Void) throws {}
    func test(over: Int, other: String = "", block: @escaping (Int) throws -> Void) throws {}

    func performMember(v: Int, block: @escaping (Int) throws -> Void) async throws {
      try await test(over: v, block: block) // Ok
    }
  }
}
