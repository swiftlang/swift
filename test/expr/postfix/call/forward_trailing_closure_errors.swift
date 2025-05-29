// RUN: %target-typecheck-verify-swift -swift-version 6

func forwardMatchWithGeneric<T>( // expected-note{{'forwardMatchWithGeneric(closure1:closure2:)' declared here}}
  closure1: T,
  closure2: () -> Int = { 5 }
) { }

func testKnownSourceBreaks(i: Int) {
  forwardMatchWithGeneric { i } // expected-error{{missing argument for parameter 'closure1' in call}}
  let _: (() -> ()).Type = type { }
}

func testUnlabeledParamMatching(i: Int, fn: ((Int) -> Int) -> Void) {
  var arrayOfFuncs = [() -> Void]()
  arrayOfFuncs.append { print("Hi") } // okay because the parameter is unlabeled?

  fn { $0 + i} // okay because the parameter label is empty
}

func forwardMatchFailure(
  onError: ((any Error) -> Void)? = nil,
  onCompletion: (Int) -> Void
) { }

func testForwardMatchFailure() {
  forwardMatchFailure { x in
    print(x)
  }
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

struct BlockObserver {
  var startHandler: ((Any) -> Void)? = nil
  var produceHandler: ((Any, Any) -> Void)? = nil
  var finishHandler: ((Any, Any, Any) -> Void)? = nil
}

func testBlockObserverExample() {
  // This was valid under the backwards scan rule in Swift 5 but is no longer valid in Swift 6
  _ = BlockObserver { _, _, _ in } // expected-error {{contextual closure type '(Any) -> Void' expects 1 argument, but 3 were used in closure body}}
  
  _ = BlockObserver { _ in } produceHandler: { _, _ in }
  _ = BlockObserver { _ in } finishHandler: { _, _, _ in }
  
  _ = BlockObserver { _ in }
    produceHandler: { _, _ in }
    finishHandler: { _, _, _ in }
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

// In Swift 5 mode either f or g can be used as a trailing closure.
// In Swift 6 mode only f can be used as a trailing closure.
func trailingClosureEitherDirection(
  f: (Int) -> Int = { $0 }, g: (Int, Int) -> Int = { $0 + $1 }
) { }

func testTrailingClosureEitherDirection() {
  trailingClosureEitherDirection { -$0 }
  trailingClosureEitherDirection { $0 * $1 } // expected-error{{contextual closure type '(Int) -> Int' expects 1 argument, but 2 were used in closure body}}
}

// This example was allowed as a trailing closure in Swift 5 mode but is no longer allowed in Swift 6 mode
struct AccidentalReorder {
  let content: () -> Int
  var optionalInt: Int?
}

func testAccidentalReorder() {
  _ = AccidentalReorder(optionalInt: 17) { 42 } // expected-error{{incorrect argument label in call (have 'optionalInt:_:', expected 'content:optionalInt:')}}
}
