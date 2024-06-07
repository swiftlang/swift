// RUN: %target-typecheck-verify-swift

func doSomething(onError: ((Error) -> Void)? = nil, onCompletion: (Int) -> Void) { }

func testDoSomething() {
  // Okay because we skip the onError.
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
func trailingClosureEitherDirection( // expected-note{{'trailingClosureEitherDirection(f:g:)' declared here}}
  f: (Int) -> Int = { $0 }, g: (Int, Int) -> Int = { $0 + $1 }
) { }

func testTrailingClosureEitherDirection() {
  trailingClosureEitherDirection { -$0 }
  trailingClosureEitherDirection { $0 * $1 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'g' to suppress this warning}}{{33-33=(g: }}{{45-45=)}}
}

// Check that we resolve ambiguities when both directions can be matched.
// expected-note@+1{{declared here}}
func trailingClosureBothDirections(
  f: (Int, Int) -> Int = { $0 + $1 }, g: (Int, Int) -> Int = { $0 - $1 }
) { }
trailingClosureBothDirections { $0 * $1 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'g' to suppress this warning}}

// Check an amusing quirk of the "backward" rule that allows the order of
// arguments to be swapped.
struct AccidentalReorder { // expected-note{{'init(content:optionalInt:)' declared here}}
  let content: () -> Int
  var optionalInt: Int?
}

func testAccidentalReorder() {
  _ = AccidentalReorder(optionalInt: 17) { 42 } // expected-warning{{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'content' to suppress this warning}}
}

func sheet(
  isPresented: Bool,
  onDismiss: (() -> Void)? = nil,
  content: () -> String
) -> String {
  content()
}

func testSwiftUISheetExample() {
  _ = sheet(isPresented: true) {
    "Hello world"
  }

  _ = sheet(isPresented: true)  {
    print("Was dismissed")
  } content: {
    "Hello world"
  }
}

// https://github.com/apple/swift/issues/65921
func issue_65921(onStart: ((String) -> Void)? = nil, onEnd: (String) -> Void) { }

func testIssue65921() {
  issue_65921 { end in
    _ = end
  }

  issue_65921 { start in
    _ = start
  } onEnd: { end in
    _ = end
  }
}

struct BlockObserver { // expected-note {{'init(startHandler:produceHandler:finishHandler:)' declared here}}
  var startHandler: ((Any) -> Void)? = nil
  var produceHandler: ((Any, Any) -> Void)? = nil
  var finishHandler: ((Any, Any, Any) -> Void)? = nil
}

func testBlockObserverExample() {
  _ = BlockObserver { _, _, _ in } // expected-warning {{backward matching of the unlabeled trailing closure is deprecated; label the argument with 'finishHandler' to suppress this warning}}
  
  _ = BlockObserver { _ in } produceHandler: { _, _ in }
  _ = BlockObserver { _ in } finishHandler: { _, _, _ in }
  
  _ = BlockObserver { _ in }
    produceHandler: { _, _ in }
    finishHandler: { _, _, _ in }
}