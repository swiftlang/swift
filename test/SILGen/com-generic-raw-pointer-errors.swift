// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-silgen -verify %s
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -enable-sil-opaque-values -I %t -emit-silgen -verify %s

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}
protocol Native {}

func borrow<T>(_ value: borrowing T) -> Builtin.RawPointer {
  Builtin.bridgeToRawPointer(value)
  // expected-error@-1 {{invalid use of builtin: bridgeToRawPointer operand must have an object or interface pointer representation}}
}

func retain<T>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.bridgeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: bridgeFromRawPointer result must have an object or interface pointer representation}}
}

func retainNative<T: Native>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.bridgeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: bridgeFromRawPointer result must have an object or interface pointer representation}}
}

func retainOptional<T: IItem>(_ pointer: Builtin.RawPointer) -> T? {
  Builtin.bridgeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: bridgeFromRawPointer result must have an object or interface pointer representation}}
}

func retainPair<T: IItem>(_ pointer: Builtin.RawPointer) -> (T, T) {
  Builtin.bridgeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: bridgeFromRawPointer result must have an object or interface pointer representation}}
}
