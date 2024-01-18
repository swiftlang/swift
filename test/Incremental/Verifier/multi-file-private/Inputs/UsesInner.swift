// expected-provides{{Inner}}
// expected-provides{{defaultFoo}}
// expected-provides{{blah}}
// expected-provides{{Foo}}
// expected-provides{{??}}
public func blah(foo: Foo?) {
  blah(foo: foo ?? defaultFoo)
}


// expected-member {{Swift.Optional<Wrapped>.deinit}}
// expected-member {{main.Inner.deinit}}
