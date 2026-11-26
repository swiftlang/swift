// RUN: %target-swift-frontend \
// RUN:     -disable-availability-checking \
// RUN:     -target %target-swift-5.9-abi-triple \
// RUN:     -emit-sil -verify \
// RUN:     -enable-experimental-feature LifetimeDependence \
// RUN:     -enable-experimental-feature Embedded \
// RUN:     %s

// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: embedded_stdlib

// NOTE: The pointer here is to give the type a lifetime dependency.
struct NoEscapeNoCopy: ~Escapable, ~Copyable {
  let p: UnsafeRawPointer?
}

protocol Foo {
  var bar: NoEscapeNoCopy {get}
}

public struct Baz: Foo {
  var bar: NoEscapeNoCopy {
    NoEscapeNoCopy(p: nil) // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note@-1 {{it depends on the lifetime of this parent value}}
  } // expected-note {{this use causes the lifetime-dependent value to escape}}
}
