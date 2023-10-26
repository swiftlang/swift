// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name test -enable-experimental-feature TypedThrows

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

func addOrThrowUntyped(_ i: Int, _ j: Int) throws -> Int { i + j }
func addOrThrowMyError(_ i: Int, _ j: Int) throws(MyError) -> Int { i + j }

func testMapArray(numbers: [Int]) {
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
