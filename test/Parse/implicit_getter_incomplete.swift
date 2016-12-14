// RUN: %target-typecheck-verify-swift

func test1() {
  var a : Int {
#if arch(x86_64)
    return 0
#else
    return 1
#endif
  }
}

// Would trigger assertion when AST verifier checks source ranges ("child source range not contained within its parent")
func test2() { // expected-note {{match}}
  var a : Int { // expected-note {{match}}
    switch i { // expected-error {{unresolved identifier}}
} // expected-error {{'switch' statement body must have at least one}}
// expected-error@+1 2 {{expected '}'}}
