// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t/enum_with_raw_type.swiftmodule %S/Inputs/enum_with_raw_type.swift
// RUN: %target-swift-frontend -I %t -typecheck -verify %s

import enum_with_raw_type

// expected-error@+1{{redundant conformance of 'Foo' to protocol 'RawRepresentable'}}
extension Foo: RawRepresentable {}
