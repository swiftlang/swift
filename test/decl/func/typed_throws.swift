// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name test

// Don't complain about <<error type>> not conforming to Error
func invalidThrownType() throws(DoesNotExist) {}
// expected-error@-1 {{cannot find type 'DoesNotExist' in scope}}

// expected-note@+1{{type declared here}}
enum MyError: Error {
  case fail
}

enum MyOtherError: Error {
  case fail
}

enum MyBadError {
  case epicFail
}

// Thrown types must conform to Error
func testBadThrownError() throws(MyBadError) { }
// expected-error@-1{{thrown type 'MyBadError' does not conform to the 'Error' protocol}}

typealias BadThrowsMyError = () throws(MyBadError) -> Void
// expected-error@-1{{thrown type 'MyBadError' does not conform to the 'Error' protocol}}

func hasThrownMyError() throws(MyError) { }

typealias ThrowsMyError = () throws(MyError) -> Void

func testThrownMyErrorType() {
  let _: ThrowsMyError = hasThrownMyError
  let _: () -> Void = hasThrownMyError
  // expected-error@-1{{invalid conversion from throwing function of type '() throws(MyError) -> ()' to non-throwing function type '() -> Void'}}
}

func throwsGeneric<T: Error>(errorType: T.Type) throws(T) { }

func throwsBadGeneric<T>(errorType: T.Type) throws(T) { }

func throwsUnusedInSignature<T: Error>() throws(T) { }
// expected-error@-1{{generic parameter 'T' is not used in function signature}}


func testSubstitutedType() {
  let _: (_: MyError.Type) -> Void = throwsGeneric
  // expected-error@-1{{invalid conversion of thrown error type 'MyError' to 'Never'}}

  let _: (_: (any Error).Type) -> Void = throwsGeneric
  // expected-error@-1{{invalid conversion of thrown error type 'any Error' to 'Never'}}

  let _: (_: MyOtherError.Type) throws(MyError) -> Void = throwsGeneric
  // expected-error@-1{{invalid conversion of thrown error type 'MyOtherError' to 'MyError'}}

  let _: (_: Never.Type) -> Void = throwsGeneric
}


func testThrowingInFunction(cond: Bool, cond2: Bool) throws(MyError) {
  if cond {
    throw MyError.fail
  } else if cond2 {
    throw .fail
  } else {
    throw MyBadError.epicFail
    // expected-error@-1{{thrown expression type 'MyBadError' cannot be converted to error type 'MyError'}}
  }
}

public func testThrowingInternal() throws(MyError) { }
// expected-error@-1{{function cannot be declared public because its thrown error uses an internal type}}

// A form of "map" for arrays that carries the error type from the closure
// through to the function itself, as a more specific form of rethrows.
func mapArray<T, U, E: Error>(_ array: [T], body: (T) throws(E) -> U) throws(E) -> [U] {
  var resultArray: [U] = .init()
  for value in array {
    resultArray.append(try body(value))
  }
  return resultArray
}

struct Person {
  var name: String
}

func addOrThrowUntyped(_ i: Int, _ j: Int) throws -> Int { i + j }
func addOrThrowMyError(_ i: Int, _ j: Int) throws(MyError) -> Int { i + j }

func testMapArray(numbers: [Int], friends: [Person]) {
  // Note: try is not required, because this throws Never
  _ = mapArray(numbers) { $0 + 1 }

  do {
    _ = try mapArray(numbers) { try addOrThrowUntyped($0, 1) }
  } catch {
    let _: Int = error // expected-error{{cannot convert value of type 'any Error'}}
  }

  do {
    _ = try mapArray(numbers) { try addOrThrowMyError($0, 1) }
  } catch {
    let _: Int = error // expected-error{{cannot convert value of type 'any Error' to specified type 'Int'}}
                       // TODO: with better inference, we infer MyError
  }

  do {
    _ = try mapArray(numbers) { (x) throws(MyError) in try addOrThrowMyError(x, 1) }
  } catch {
    let _: Int = error // expected-error{{cannot convert value of type 'MyError' to specified type 'Int'}}
  }

  do {
    // Keypath-as-function
    _ = mapArray(friends, body: \Person.name)
  }
}

// Inference of Error conformance from the use of a generic parameter in typed
// throws.
func requiresError<E: Error>(_: E.Type) { }

func infersThrowing<E>(_ error: E.Type) throws(E) {
  requiresError(error)
}

func infersThrowingNested<E>(_ body: () throws(E) -> Void) {
  requiresError(E.self)
}

struct HasASubscript {
  subscript<E>(_: E.Type) -> Int {
    get throws(E) {
      requiresError(E.self)
      return 0
    }
  }
}

// expected-error@+1{{thrown type 'any Codable & Error' (aka 'any Decodable & Encodable & Error') does not conform to the 'Error' protocol}}
func throwCodableErrors() throws(any Codable & Error) { }

enum Either<First, Second> {
case first(First)
case second(Second)
}

extension Either: Error where First: Error, Second: Error { }

func f<E1, E2>(_ error: Either<E1, E2>) throws(Either<E1, E2>) {
  throw error
}

// Ensure that calls to 'rethrows' functions are always treated as throwing `any
// Error`.
func rethrowingFunc(body: () throws -> Void) rethrows { }

func typedCallsRethrowingFunc<E>(body: () throws(E) -> Void) throws(E) {
  try rethrowingFunc(body: body) // expected-error{{thrown expression type 'any Error' cannot be converted to error type 'E'}}
}

// Compatibility feature: calls from a rethrows function to a rethrows-like
// function using typed throws are permitted.
func rethrowsLike<E>(_ body: () throws(E) -> Void) throws(E) { }

protocol P { }

func notRethrowsLike1<E: P>(_ body: () throws(E) -> Void) throws(E) { }
// expected-note@-1{{required by global function 'notRethrowsLike1' where 'E' = 'any Error'}}

func notRethrowsLike2<E>(_ body: () throws(E) -> Void) throws { }
func notRethrowsLike3<E>(_ body: () throws(E) -> Void, defaulted: () throws -> Void = {}) throws(E) { }

func fromRethrows(body: () throws -> Void) rethrows {
  try rethrowsLike(body)

  try rethrowsLike(hasThrownMyError) // expected-error{{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  // expected-note@-1{{call is to 'rethrows' function, but argument function can throw}}

  try notRethrowsLike1(body) // expected-error{{type 'any Error' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}

  try notRethrowsLike2(body) // expected-error{{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  try notRethrowsLike3(body) // expected-error{{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
}

// Substitution involving 'any Error' or 'Never' thrown error types should
// use untyped throws or be non-throwing.
enum G_E<T> {
  case tuple((x: T, y: T))
}

func testArrMap(arr: [String]) {
  _ = mapArray(arr, body: G_E<Int>.tuple)
  // expected-error@-1{{conflicting arguments to generic parameter 'T' ('String' vs. '(x: Int, y: Int)')}}
}

// Shadowing of typed-throws Result.get() addresses a source compatibility
// issue with the introduction of typed throws.
extension Result {
  public func dematerialize() throws -> Success {
    return try get()
  }

  public func get() throws -> Success {
    switch self {
    case let .success(value):
      return value
    case let .failure(error):
      throw error
    }
  }
}

struct NotAnError<T> {}

func badThrowingFunctionType<T>(_: () throws(NotAnError<T>) -> ()) {}
// expected-error@-1 {{thrown type 'NotAnError<T>' does not conform to the 'Error' protocol}}
