// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportUnsafeCxxMethodsAsAlwaysUnsafe

// REQUIRES: swift_feature_ImportUnsafeCxxMethodsAsAlwaysUnsafe

// 'Container.insert' returns an unsafe projection, so it is renamed to
// '__insertUnsafe()'. The recommended way to make such a method usable is to
// hand-write a same-named safe wrapper that calls the renamed spelling -- this
// is what the C++ standard library overlay does for 'CxxSet.insert(_:)'.
//
// With ImportUnsafeCxxMethodsAsAlwaysUnsafe the C++ method also keeps its
// original name (as '@unsafe(always)'), which would collide with such a
// wrapper. The unsafe import is '@_disfavoredOverload' so that it doesn't.

import AlwaysUnsafeSafeWrapper

extension Container {
  @discardableResult
  mutating func insert(_ value: Int32) -> Element {
    // The migration stub points the wrapper at the very name it is defining.
    unsafe __insertUnsafe(value).pointee
    // expected-warning@-1 {{'__insertUnsafe' is deprecated: renamed to 'insert(_:)'}}
    // expected-note@-2 {{use 'insert(_:)' instead}}
  }
}

func useWrapper(_ c: inout Container) {
  // Resolves to the safe wrapper, and so needs no 'unsafe' acknowledgement.
  let e = c.insert(1)
  let _: Element = e

  // The disfavored unsafe import is still reachable when asked for explicitly,
  // and is still '@unsafe(always)' when it is.
  let p: UnsafeMutablePointer<Element>? = c.insert(2)
  // expected-error@-1 {{expression uses constructs that are very hard to use correctly and must be marked with 'unsafe'}}
  // expected-note@-2 {{reference to unsafe instance method 'insert'}}
  _ = p

  let q: UnsafeMutablePointer<Element>? = unsafe c.insert(3)
  _ = q

  // Unambiguous members are unaffected.
  _ = c.count()
}
