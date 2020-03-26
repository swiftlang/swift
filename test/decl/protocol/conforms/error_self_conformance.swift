// RUN: %target-typecheck-verify-swift

func wantsError<T: Error>(_: T) {}
// expected-note@-1 {{required by global function 'wantsError' where 'T' = 'ErrorRefinement'}}
// expected-note@-2 {{required by global function 'wantsError' where 'T' = 'Error & OtherProtocol'}}
// expected-note@-3 {{required by global function 'wantsError' where 'T' = 'C & Error'}}

func testSimple(error: Error) {
  wantsError(error)
}

protocol ErrorRefinement : Error {}
func testErrorRefinment(error: ErrorRefinement) {
  wantsError(error) // expected-error {{value of protocol type 'ErrorRefinement' cannot conform to 'Error'; only struct/enum/class types can conform to protocols}}
}

protocol OtherProtocol {}
func testErrorComposition(error: Error & OtherProtocol) {
  wantsError(error) // expected-error {{value of protocol type 'Error & OtherProtocol' cannot conform to 'Error'; only struct/enum/class types can conform to protocols}}
}

class C {}
func testErrorCompositionWithClass(error: Error & C) {
  wantsError(error) // expected-error {{value of protocol type 'C & Error' cannot conform to 'Error'; only struct/enum/class types can conform to protocols}}
}
