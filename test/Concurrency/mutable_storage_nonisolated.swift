// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

@propertyWrapper struct P {
  var wrappedValue = 0
}

class NonSendable {}

struct ImplicitlySendable {
  var a: Int

  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }

  // okay
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

struct ImplicitlyNonSendable {
  let a: NonSendable

  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }

  // not okay
  nonisolated var d = 0  // expected-error {{'nonisolated' cannot be applied to mutable stored properties}}
  // expected-note@-1 {{convert 'd' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

public struct PublicSendable: Sendable {
  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }

  // okay
  nonisolated var d = 0

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}

public struct PublicNonSendable {
  // always okay
  nonisolated let b = 0
  nonisolated var c: Int { 0 }

  // not okay
  nonisolated var d = 0  // expected-error {{'nonisolated' cannot be applied to mutable stored properties}}
  // expected-note@-1 {{convert 'd' to a 'let' constant or consider declaring it 'nonisolated(unsafe)' if manually managing concurrency safety}}

  // never okay
  nonisolated lazy var e = 0  // expected-error {{'nonisolated' is not supported on lazy properties}}
  @P nonisolated var f = 0  // expected-error {{'nonisolated' is not supported on properties with property wrappers}}
}
