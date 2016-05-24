// RUN: %target-parse-verify-swift

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
  var a : Int { // expected-note {{match}} expected-note {{did you mean 'a'?}}
    switch i { // expected-error {{unresolved identifier}}
} // expected-error {{'switch' statement body must have at least one}}
// expected-error 2 {{expected '}'}}