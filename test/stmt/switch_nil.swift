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
