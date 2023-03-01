// RUN: %target-typecheck-verify-swift

// rdar://105781521
enum MyEnum {
  case first(String)
}

func takeClosure(_ x: () -> Void) {}

func test(value: MyEnum) {
  takeClosure {
    switch value {
    case .first(true):
      // expected-error@-1 {{expression pattern of type 'Bool' cannot match values of type 'String'}}
      // expected-note@-2 {{overloads for '~=' exist with these partially matching parameter lists: (Substring, String)}}
      break
    default:
      break
    }
  }
}
