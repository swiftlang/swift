// RUN: %target-typecheck-verify-swift -parse-as-library

enum MyError: Error {
case failed
}

enum HomeworkError: Error {
case dogAteIt
}

class SuperError: Error { }
class SubError: SuperError { }

protocol VeryThrowing {
  func f() throws
  func g() throws(MyError)
  func h() throws(HomeworkError) // expected-note{{protocol requires function 'h()' with type '() throws(HomeworkError) -> ()'}}
  func i() throws(SuperError)

  var prop1: Int { get throws }
  var prop2: Int { get throws(MyError) }
  var prop3: Int { get throws(HomeworkError) } // expected-note{{protocol requires property 'prop3' with type 'Int'}}
                                               // FIXME: poor diagnostic above
  var prop4: Int { get throws(SuperError) }
}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1{{type 'ConformingToVeryThrowing' does not conform to protocol 'VeryThrowing'}}
struct ConformingToVeryThrowing: VeryThrowing {
  func f() throws(MyError) { } // okay to make type more specific
  func g() { } // okay to be non-throwing
  func h() throws(MyError) { } // expected-note{{candidate throws, but protocol does not allow it}}
                               // FIXME: Diagnostic above should be better
  func i() throws(SubError) { } // okay to have a subtype

  var prop1: Int { get throws(MyError) { 0 } }
  var prop2: Int { 0 } // okay to be non-throwing
  var prop3: Int { get throws(MyError) { 0 } } // expected-note{{candidate throws, but protocol does not allow it}}
                               // FIXME: Diagnostic above should be better
  var prop4: Int { get throws(SubError) { 0 } }
}

// Associated type inference.
protocol FailureAssociatedType {
  associatedtype Failure: Error

  func f() throws(Failure)
}

struct S1: FailureAssociatedType {
  func f() throws(MyError) { }
}

struct S2: FailureAssociatedType {
  func f() throws { }
}

struct S3: FailureAssociatedType {
  func f() { }
}

func testAssociatedTypes() {
  let _ = S1.Failure() // expected-error{{'S1.Failure' (aka 'MyError') cannot be constructed because it has no accessible initializers}}
  let _ = S2.Failure() // expected-error{{'S2.Failure' (aka 'any Error') cannot be constructed because it has no accessible initializers}}
  let _: Int = S3.Failure() // expected-error{{cannot convert value of type 'S3.Failure' (aka 'Never') to specified type 'Int'}}
  // expected-error@-1{{missing argument for parameter 'from' in call}}
}

// Make sure we can throw the generic failure type.
func assocFailureType<T: FailureAssociatedType>(_ value: T, _ error: T.Failure) throws(T.Failure) {
  throw error
}

// Allow a typed-throws version of a function to witness a rethrowing function.
public protocol HasRethrowingMap: Sequence {
  func map<T>(_ transform: (Element) throws -> T) rethrows -> [T]
}

extension Array: HasRethrowingMap {}

// rdar://149438520 -- incorrect handling of subtype relation between type parameter and Never
protocol DependentThrowing {
  associatedtype E: Error
  func f() throws(E)
}

extension DependentThrowing {
  func f() {}
}

struct DefaultDependentThrowing: DependentThrowing {
  typealias E = Error
}