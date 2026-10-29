// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes
// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-sil-opaque-values

// REQUIRES: swift_feature_Lifetimes

struct S: ~Escapable {
  let pointer: UnsafeRawPointer?

  @_lifetime(borrow pointer)
  init(_unchecked pointer: UnsafeRawPointer?) {
    self.pointer = pointer
  }
}

// The converted pointer is the function's argument, so the result depends on the argument.
@_lifetime(borrow pointer)
func make(_ pointer: UnsafePointer<Int>) -> S {
  S(_unchecked: pointer)
}

// A dependence that does escape must still be diagnosed through the conversion:
// the converted pointer is local to the function, not the argument that the
// result is declared to depend on.
@_lifetime(borrow arg)
func escapesLocal(_ arg: UnsafePointer<Int>) -> S {
  let local = UnsafePointer<Int>(bitPattern: 0x1000)! // expected-note {{it depends on the lifetime of variable 'local'}}
  return S(_unchecked: local) // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{this use causes the lifetime-dependent value to escape}}
}
