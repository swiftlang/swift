// RUN: %target-typecheck-verify-swift -disable-fuzzy-forward-scan-trailing-closure-matching

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

// When "fuzzy matching is disabled, this will fail.
func forwardMatchFailure( // expected-note{{declared here}}
  onError: ((Error) -> Void)? = nil,
  onCompletion: (Int) -> Void
) { }

func testForwardMatchFailure() {
  forwardMatchFailure { x in
    print(x)
  } // expected-error{{missing argument for parameter 'onCompletion' in call}}{{4-4= onCompletion: <#(Int) -> Void#>}}
}
