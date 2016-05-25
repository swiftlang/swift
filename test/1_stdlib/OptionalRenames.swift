// RUN: %target-parse-verify-swift

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

