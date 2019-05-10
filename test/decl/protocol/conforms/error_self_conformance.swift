// RUN: %target-typecheck-verify-swift

func wantsError<T: Error>(_: T) {}

func testSimple(error: Error) {
  wantsError(error)
}

protocol ErrorRefinement : Error {}
func testErrorRefinment(error: ErrorRefinement) {
  wantsError(error) // expected-error {{protocol type 'ErrorRefinement' cannot conform to 'Error' because only concrete types can conform to protocols}}
}

protocol OtherProtocol {}
func testErrorComposition(error: Error & OtherProtocol) {
  wantsError(error) // expected-error {{protocol type 'Error & OtherProtocol' cannot conform to 'Error' because only concrete types can conform to protocols}}
}

class C {}
func testErrorCompositionWithClass(error: Error & C) {
  wantsError(error) // expected-error {{protocol type 'C & Error' cannot conform to 'Error' because only concrete types can conform to protocols}}
}
