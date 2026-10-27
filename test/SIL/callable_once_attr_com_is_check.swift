// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %if swift_feature_NoncopyableCasting %{ %target-swift-frontend %s -enable-experimental-feature NoncopyableCasting \
// RUN:     -emit-sil -verify \
// RUN:     -target %target-future-triple \
// RUN:     -enable-experimental-com-interop -I %t \
// RUN:     -enable-experimental-feature OnceFunctions \
// RUN: %}

// REQUIRES: swift_feature_OnceFunctions
// REQUIRES: PTRSIZE=64

// SILGen lowers an `is` check that involves a COM existential to a cast that
// consumes its operand, so an at-most-once autoclosure consumes the capture.

import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget {}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol INoncopyableWidget: ~Copyable {}

protocol Shape: ~Copyable {}
struct Square: ~Copyable {}

func andAtMostOnce(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () -> Bool
) -> Bool {
  lhs ? rhs() : false
}

func testIsCheckToCOMExistentialConsumesCapture(
  _ shape: consuming any Shape & ~Copyable
) {
  _ = andAtMostOnce(true, shape is any IWidget)
}

func testUseAfterIsCheckToCOMExistential(
  _ shape: consuming any Shape & ~Copyable
  // expected-error@-1 {{'shape' consumed more than once}}
) {
  _ = andAtMostOnce(true, shape is any IWidget) // expected-note {{consumed here}}
  _ = consume shape // expected-note {{consumed again here}}
}

func testIsCheckOfCOMExistentialConsumesCapture(
  _ widget: consuming any INoncopyableWidget & ~Copyable
) {
  _ = andAtMostOnce(true, widget is Square)
  // expected-warning@-1 {{cast from 'any INoncopyableWidget & ~Copyable' to unrelated type 'Square' always fails}}
}

func testUseAfterIsCheckOfCOMExistential(
  _ widget: consuming any INoncopyableWidget & ~Copyable
  // expected-error@-1 {{'widget' consumed more than once}}
) {
  _ = andAtMostOnce(true, widget is Square) // expected-note {{consumed here}}
  // expected-warning@-1 {{cast from 'any INoncopyableWidget & ~Copyable' to unrelated type 'Square' always fails}}
  _ = consume widget // expected-note {{consumed again here}}
}
