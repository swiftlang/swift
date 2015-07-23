func use(x: Int) {}
func test() {
  use(a!)
  use(b) // expected-error {{use of unresolved identifier 'b'}}
}
