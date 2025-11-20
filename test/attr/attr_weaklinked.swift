// RUN: %target-typecheck-verify-swift

// UNSUPPORTED: OS=windows-msvc

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
