// RUN: %target-typecheck-verify-swift

func wantsError<T: Error>(_: T) {}
// expected-note@-1 {{required by global function 'wantsError' where 'T' = 'any ErrorRefinement'}}
// expected-note@-2 {{required by global function 'wantsError' where 'T' = 'any Error & OtherProtocol'}}
// expected-note@-3 {{required by global function 'wantsError' where 'T' = 'any C & Error'}}

func testSimple(error: Error) {
  wantsError(error)
}

protocol ErrorRefinement : Error {}
func testErrorRefinment(error: ErrorRefinement) {
  wantsError(error) // expected-error {{type 'any ErrorRefinement' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}

protocol OtherProtocol {}
func testErrorComposition(error: Error & OtherProtocol) {
  wantsError(error) // expected-error {{type 'any Error & OtherProtocol' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}

class C {}
func testErrorCompositionWithClass(error: Error & C) {
  wantsError(error) // expected-error {{type 'any C & Error' cannot conform to 'Error'}} expected-note {{only concrete types such as structs, enums and classes can conform to protocols}}
}
