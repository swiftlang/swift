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
      guard let x = UnsafeMutablePointer<Double>(OpaquePointer(self.p)) else {
        return
      }
      _ = x

      guard let x = UnsafeMutablePointer<Double>(OpaquePointer(p)) else {
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
