// RUN: %target-typecheck-verify-swift -swift-version 5 -experimental-multi-statement-closures -enable-experimental-static-assert

func isInt<T>(_ value: T) -> Bool {
  return value is Int
}

func maybeGetValue<T>(_ value: T) -> T? {
  return value
}

enum MyError: Error {
  case featureIsTooCool

  func doIt() { }
}

enum State {
  case suspended
  case partial(Int, Int)
  case finished
}

func random(_: Int) -> Bool { return false }

func mightThrow() throws -> Bool { throw MyError.featureIsTooCool }

func mapWithMoreStatements(ints: [Int], state: State) throws {
  let _ = try ints.map { i in
    guard var actualValue = maybeGetValue(i) else {
      return String(0)
    }

    let value = actualValue + 1
    do {
      if isInt(i) {
        print(value)
      } else if value == 17 {
        print("seventeen!")
      }
    }

    while actualValue < 100 {
      actualValue += 1
    }

  my_repeat:
    repeat {
      print("still here")

      if i % 7 == 0 {
        break my_repeat
      }
    } while random(i)

    defer {
      print("I am so done here")
    }

    for j in 0..<i where j % 2 == 0 {
      if j % 7 == 0 {
        continue
      }

      switch (state, j) {
      case (.suspended, 0):
        print("something")
        fallthrough
      case (.finished, 0):
        print("something else")

      case (.partial(let current, let end), let j):
        print("\(current) of \(end): \(j)")

      default:
        print("so, here we are")
      }
      print("even")
      throw MyError.featureIsTooCool
    }

    #assert(true)

    // expected-warning@+1{{danger zone}}
    #warning("danger zone")

#if false
    struct NothingHere { }
#else
    struct NestedStruct {
      var x: Int
    }
#endif

    do {
      print(try mightThrow())
    } catch let e as MyError {
      e.doIt()
    } catch {
      print(error)
    }
    return String(value)
  }
}

func acceptsWhateverClosure<T, R>(_ value: T, _ fn: (T) -> R) { }

func testReturnWithoutExpr(i: Int) {
  acceptsWhateverClosure(i) { i in
    print(i)
    return
  }
}

// `withContiguousStorageIfAvailable` is overloaded, so let's make sure that
// filtering works correctly.
func test_overloaded_call(arr: [Int], body: (UnsafeBufferPointer<Int>) -> Void) -> Void {
  arr.withContiguousStorageIfAvailable { buffer in
    let _ = type(of: buffer)
    body(buffer) // ok
  }
}

// Used to wrap closure in `FunctionConversionExpr` in this case,
// but now solver would just inject return expression into optional where necessary.
func test_result_optional_injection() {
  func fn<T>(_: () -> T?) -> [T] {
    []
  }

  _ = fn {
    if true {
      return // Ok
    }
  }
}

let _ = {
  for i: Int8 in 0 ..< 20 { // Ok (pattern can inform a type of the sequence)
    print(i)
  }
}

func test_workaround_for_optional_void_result() {
  func test<T>(_: (Int?) -> T?) {}

  test {
    guard let x = $0 else {
      return // Ok
    }

    print(x)
  }

  test {
    if $0! > 0 {
      return
    }

    let _ = $0
  }

  func test_concrete(_: (Int) -> Void?) {
  }

  test_concrete {
    guard let x = Optional($0) else {
      return // Ok
    }

    print(x)
  }

  test_concrete {
    if $0 > 0 {
      return // Ok
    }

    let _ = $0
  }
}

enum WrapperEnum<Wrapped> where Wrapped: RawRepresentable {
case known(Wrapped)

  static func ~= (lhs: Wrapped, rhs: WrapperEnum<Wrapped>) -> Bool where Wrapped: Equatable {
    switch rhs {
    case .known(let wrapped):
      return wrapped == lhs
    }
  }
}

func test_custom_tilde_equals_operator_matching() {
  enum TildeTest : String {
  case test = "test"
  case otherTest = ""
  }

  func test(_: (WrapperEnum<TildeTest>) -> Void) {}

  test { v in
    print(v)

    switch v {
    case .test: break // Ok although `.test` comes from `TildeTest` instead of `WrapperEnum`
    case .otherTest: break // Ok although `.otherTest` comes from `TildeTest` instead of `WrapperEnum`
    case .known(_): break // Ok - `.known` comes from `WrapperEnum`
    }
  }
}

// Local functions can capture variables before they are declared.
func test_local_function_capturing_vars() {
  struct A {
    var cond: Bool
  }

  func test<T>(fn: () -> T) -> T {
    fn()
  }

  func outer(a: A) {
    test {
      func local() {
        if !message.isEmpty { // Ok
          print(message)
        }

        message = "World" // Ok
      }

      var message = a.cond ? "hello" : ""
    }
  }
}

func test_pattern_ambiguity_doesnot_crash_compiler() {
  enum E {
  case hello(result: Int) // expected-note 2 {{found this candidate}}
  case hello(status: Int) // expected-note 2 {{found this candidate}}
  }

  let _: (E) -> Void = {
    switch $0 {
    case .hello(_): break // expected-error {{ambiguous use of 'hello'}}
    }
  }

  let _: (E) -> Void = {
    switch $0 {
    case let E.hello(x): print(x) // expected-error {{ambiguous use of 'hello'}}
    default: break
    }
  }
}

func test_taps_type_checked_with_correct_decl_context() {
  struct Path {
    func contains<T>(_: T) -> Bool where T: StringProtocol { return false }
  }

  let paths: [Path] = []
  let strs: [String] = []

  _ = paths.filter { path in
    for str in strs where path.contains("\(str).hello") {
      return true
    }
    return false
  }
}

// rdar://90347159 - in pattern matching context `case` should be preferred over static declarations
func test_pattern_matches_only_cases() {
  enum ParsingError : Error {
    case ok(Int)
    case failed([Error], Int)

    static var ok: Int { 42 }
    static func failed(_: [Error], at: Any) -> Self { fatalError() }
  }

  let _: (ParsingError) -> Void = {
    switch $0 {
    case let ParsingError.failed(errors, _): print(errors) // Ok
    default: break
    }

    switch $0 {
    case let ParsingError.ok(result): print(result) // Ok
    default: break
    }
  }
}

// rdar://91225620 - type of expression is ambiguous without more context in closure
func test_wrapped_var_without_initializer() {
  @propertyWrapper
  struct Wrapper {
    private let name: String

    var wrappedValue: Bool {
      didSet {}
    }

    init(name: String) {
      self.wrappedValue = false
      self.name = name
    }
  }

  func fn(_: () -> Void) {}

  fn {
    @Wrapper(name: "foo")
    var v;
  }
}
