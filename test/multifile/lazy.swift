// RUN: %target-swift-frontend -emit-sil -verify -primary-file %s %S/Inputs/external_lazy_property.swift

// rdar://45712204
func test1(s: Test1) {
  s.property // expected-error {{cannot use mutating getter on immutable value: 's' is a 'let' constant}}
}
