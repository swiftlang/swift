// RUN: %target-swift-emit-silgen %s -o /dev/null -verify

@_silgen_name("foo")
func a(_ x: Int) -> Int { // expected-note {{other definition here}}
  return x
}

@_silgen_name("foo")
func b(_ x: Int) -> Int { // expected-error {{multiple definitions of symbol 'foo'}}
  return x
}

@_cdecl("bar")
func c(_ x: Int) -> Int { // expected-note {{other definition here}}
  return x
}

@_cdecl("bar")
func d(_ x: Int) -> Int { // expected-error {{multiple definitions of symbol 'bar'}}
  return x
}
