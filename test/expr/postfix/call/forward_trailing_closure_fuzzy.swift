// RUN: %target-typecheck-verify-swift

func doSomething(onError: ((Error) -> Void)? = nil, onCompletion: (Int) -> Void) { }

func testDoSomething() {
  doSomething { x in
    print(x)
  }

  doSomething(onError: nil) { x in
    print(x)
  }

  doSomething { e in
    print(e)
  } onCompletion: { x in
    print(x)
  }
}

func trailingClosures(
  arg1: () -> Void,
  arg2: () -> Void = {},
  arg3: () -> Void = {}
) {}

func testTrailingClosures() {
  trailingClosures { print("Hello!") }
  trailingClosures { print("Hello,") } arg3: { print("world!") }
}

// Ensure that we can match either with the forward or the backward rule,
// depending on additional type check information.
func trailingClosureEitherDirection(
  f: (Int) -> Int = { $0 }, g: (Int, Int) -> Int = { $0 + $1 }
) { }

func testTrailingClosureEitherDirection() {
  trailingClosureEitherDirection { -$0 }
  trailingClosureEitherDirection { $0 * $1 }
}

// Check that we resolve ambiguities when both directions can be matched.
// expected-note@+1{{'trailingClosureBothDirections(f:g:)' contains defaulted closure parameters 'f' and 'g'}}
func trailingClosureBothDirections(
  f: (Int, Int) -> Int = { $0 + $1 }, g: (Int, Int) -> Int = { $0 - $1 }
) { }
trailingClosureBothDirections { $0 * $1 } // expected-warning{{since Swift 5.3, unlabeled trailing closure argument matches parameter 'f' rather than parameter 'g'}}
// expected-note@-1{{label the argument with 'g' to retain the pre-Swift 5.3 behavior}}
// expected-note@-2{{label the argument with 'f' to silence this warning for Swift 5.3 and newer}}

// Check an amusing quirk of the "backward" rule that allows the order of
// arguments to be swapped.
struct AccidentalReorder {
  let content: () -> Int
  var optionalInt: Int?
}

func testAccidentalReorder() {
  _ = AccidentalReorder(optionalInt: 17) { 42 }
}
