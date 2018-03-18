// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/enum_with_raw_type.swiftmodule %S/Inputs/enum_with_raw_type.swift
// RUN: %target-swift-frontend -I %t -typecheck -verify %s

import enum_with_raw_type

// expected-warning@+1{{conformance of 'Foo' to protocol 'RawRepresentable' was already stated in the type's module 'enum_with_raw_type'}}
extension Foo: RawRepresentable {}
