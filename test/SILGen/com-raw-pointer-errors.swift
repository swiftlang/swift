// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -enable-builtin-module -I %t -emit-silgen -verify %s

import Builtin

@com(interface: "51000000-0000-0000-0000-000000000001")
protocol IItem {}

func optional(_ pointer: Builtin.RawPointer) -> (any IItem)? {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}

// A conforming generic value does not have the existential's representation.
func generic<T: IItem>(_ pointer: Builtin.RawPointer) -> T {
  Builtin.takeFromRawPointer(pointer)
  // expected-error@-1 {{invalid use of builtin: takeFromRawPointer result must be a single reference}}
}
