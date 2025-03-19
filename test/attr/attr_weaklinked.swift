// RUN: %target-typecheck-verify-swift -enable-experimental-feature ABIAttribute

// UNSUPPORTED: OS=windows-msvc
// REQUIRES: swift_feature_ABIAttribute

@_weakLinked public func f() { }

// @_weakLinked -- banned in @abi
struct WeakLinked {
  @abi(@_weakLinked func fn1()) // expected-error {{unused '_weakLinked' attribute in '@abi'}} {{8-20=}}
  @_weakLinked func fn1() {}

  @abi(@_weakLinked func fn2()) // expected-error {{unused '_weakLinked' attribute in '@abi'}} {{8-20=}}
  func fn2() {}

  @abi(func fn3())
  @_weakLinked func fn3() {}
}
