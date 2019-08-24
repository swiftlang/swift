// RUN: %target-typecheck-verify-swift

protocol NeedsF0 {
  func f0() // expected-note {{protocol requires function 'f0()' with type '() -> ()'; do you want to add a stub?}}
}

struct S0 : NeedsF0 {  // expected-error {{type 'S0' does not conform to protocol 'NeedsF0'}}
  @_implements(NeedsF0, f0())
  func g0() -> Bool {  // expected-note {{candidate has non-matching type '() -> Bool'}}
    return true
  }

  @_implements(NeedsF0, ff0()) // expected-error {{protocol 'NeedsF0' has no member 'ff0()'}}
  func gg0() {
  }

  @_implements(Int, zz) // expected-error {{non-protocol type in @_implements attribute}}
  static func gg1(x:S0, y:S0) -> Bool {
  }

  @_implements(Equatable, ==(_:_:)) // expected-error {{containing type 'S0' does not conform to protocol 'Equatable'}}
  static func gg2(x:S0, y:S0) -> Bool {
  }
}

