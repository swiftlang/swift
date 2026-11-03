// REQUIRES: swift_swift_parser
//
// RUN: %target-typecheck-verify-swift %s

@freestanding(expression)
macro testMacro() -> Int = #externalMacro(module: "FakeMacros", type: "TestMacro")
// expected-warning@-1 {{external macro implementation type}}

func testAsyncInsteadOfAwait() async {
  // expected-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{3-8=await}}
  async #testMacro()
}
