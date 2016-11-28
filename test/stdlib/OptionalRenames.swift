// RUN: %target-typecheck-verify-swift

func getInt(x: Int) -> Int? {
  if x == 0 {
    return .None // expected-error {{'None' has been renamed to 'none'}} {{13-17=none}}
  }
  return .Some(1) // expected-error {{'Some' has been renamed to 'some'}} {{11-15=some}}
}

let x = Optional.Some(1) // expected-error {{'Some' has been renamed to 'some'}} {{18-22=some}}

switch x {
  case .None: break // expected-error {{'None' has been renamed to 'none'}} {{9-13=none}}
  case .Some(let x): print(x) // expected-error {{'Some' has been renamed to 'some'}} {{9-13=some}}
}

let optionals: (Int?, Int?) = (Optional.Some(1), .None)
// expected-error@-1 {{'Some' has been renamed to 'some'}} {{41-45=some}}
// expected-error@-2 {{'None' has been renamed to 'none'}} {{51-55=none}}

switch optionals {
  case (.None, .none): break // expected-error {{'None' has been renamed to 'none'}} {{10-14=none}}
  case (.Some(let left), .some(let right)): break // expected-error {{'Some' has been renamed to 'some'}} {{10-14=some}}
  default: break
}

