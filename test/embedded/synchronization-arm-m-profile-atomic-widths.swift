// RUN: %target-swift-frontend -typecheck -verify %s -target armv7m-apple-none-macho -enable-experimental-feature Embedded -parse-as-library -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -verify %s -target armv7em-none-none-eabi -enable-experimental-feature Embedded -parse-as-library -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -verify %s -target armv8m.main-none-none-eabi -enable-experimental-feature Embedded -parse-as-library -disable-availability-checking
// RUN: %target-swift-frontend -typecheck -verify %s -target armv8.1m.main-none-none-eabi -enable-experimental-feature Embedded -parse-as-library -disable-availability-checking

// UNSUPPORTED: CPU=wasm32
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

import Synchronization

func word(_ a: borrowing Atomic<UInt32>) -> UInt32 {
  a.add(1, ordering: .relaxed).newValue
}

func doubleWord(_ a: borrowing Atomic<UInt64>) {} // expected-error {{type 'UInt64' does not conform to protocol 'AtomicRepresentable'}}
