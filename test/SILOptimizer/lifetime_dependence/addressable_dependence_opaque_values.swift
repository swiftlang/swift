// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -verify %s
// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableTypes -enable-sil-opaque-values -verify %s

// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_Lifetimes

// With opaque values, SILGen passes an @in_guaranteed argument of a loadable
// type by value, as a load of the variable's address. A dependence on an
// addressable argument must still be on that address, so that it is tied to the
// variable's access scope. If it were on the loaded copy, the access scope would
// not be extended over the lifetime-dependent value, and a use that is within
// the variable's scope would be diagnosed while a use that is not would be
// missed.

@_addressableForDependencies
struct Buf {
  var a = 0, b = 0, c = 0, d = 0

  var view: Span<Int> {
    @_lifetime(borrow self)
    get { fatalError() }
  }
}

func usedWithinScope() -> Int {
  var buf = Buf()
  buf.a = 1
  let s = buf.view
  return s.count
}

func usedOutOfScope() -> Int {
  var s: Span<Int> // expected-error {{lifetime-dependent variable 's' escapes its scope}}
  do {
    var buf = Buf()
    buf.a = 1
    s = buf.view // expected-note {{it depends on this scoped access to variable 'buf'}}
  }
  return s.count // expected-note {{this use of the lifetime-dependent value is out of scope}}
}
