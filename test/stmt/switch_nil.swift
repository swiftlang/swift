// RUN: %target-typecheck-verify-swift

enum Hey {
  case listen
}

func test() {
  switch Hey.listen {
  case nil: // expected-warning {{type 'Hey' is not optional, value can never be nil}}
    break
  default:
    break
  }
}

struct Nilable: ExpressibleByNilLiteral {
  init(nilLiteral: ()) {}
}

func testNil() {
  // N.B. A deeply confusing case as no conversion is performed on the `nil`
  // literal. Instead, the match subject is converted to `Nilable?` and compared
  // using ~=.
  switch Nilable(nilLiteral: ()) {
  case nil: // expected-warning {{type 'Nilable' is not optional, value can never be nil}}
    break
  default:
    break
  }
}
