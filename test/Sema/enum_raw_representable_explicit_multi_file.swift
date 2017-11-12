// RUN: %target-swift-frontend -typecheck -primary-file %s %S/Inputs/enum_raw_representable_explicit_multi_file_2.swift
// RUN: %target-swift-frontend -typecheck -verify %s -primary-file %S/Inputs/enum_raw_representable_explicit_multi_file_2.swift

enum Foo: Int { case A }

extension Bar: RawRepresentable {}

enum Bas: Int { case A }
// expected-note@+1 {{'Bas' declares conformance to protocol 'RawRepresentable' here}}
extension Bas: RawRepresentable {}
