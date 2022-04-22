// RUN: %target-typecheck-verify-swift

func opensError<T: Error>(_: T) {}
func wantsError<T: Error>(_: T, _: T) {}
// expected-note@-1 3{{required by global function 'wantsError' where 'T' = 'any}}

func testSimple(error: Error) {
  opensError(error)
}

protocol ErrorRefinement : Error {}
func testErrorRefinement(error: ErrorRefinement) {
  opensError(error) // okay
  wantsError(error, error) // expected-error {{type 'any ErrorRefinement' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}

protocol OtherProtocol {}
func testErrorComposition(error: Error & OtherProtocol) {
  opensError(error) // okay
  wantsError(error, error) // expected-error {{type 'any Error & OtherProtocol' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}

class C {}
func testErrorCompositionWithClass(error: Error & C) {
  opensError(error) // okay
  wantsError(error, error) // expected-error {{type 'any C & Error' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}
