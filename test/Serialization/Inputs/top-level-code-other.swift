func use(_ x: Int) {}
func test() {
  use(a!)
  use(b) // expected-error {{cannot find 'b' in scope; did you mean 'a'?}}
}
