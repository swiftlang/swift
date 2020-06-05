// FIXME: This dependency ONLY occurs with private dependencies. Otherwise we
// rely on the interface hash changing to cause this file to be rebuilt, which
// will *not* work with type fingerprints enabled.
// See rdar://63984581
// fixme-provides{{Inner}}

// expected-provides{{defaultFoo}}
// expected-provides{{blah}}
// expected-provides{{Optional}}
// expected-provides{{Foo}}
// expected-provides{{??}}
public func blah(foo: Optional<Foo>) {
  blah(foo: foo ?? defaultFoo)
}
