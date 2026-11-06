// A 'where' clause inside the angle brackets of a generic parameter list is
// obsolete; both parsers diagnose it and offer the same fix-its.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

protocol Womparable {}
protocol Mashable {}

func f3<T where T: Womparable>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{10-30=}} {{37-37= where T: Womparable}}
func f12<T: Mashable where T: Womparable>(x: T) {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{21-41=}} {{48-48= where T: Womparable}}
func f24<T where T: Womparable>(x: T) where T: Equatable {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{11-31=}} {{39-44=where T: Womparable,}}
struct S2<T where T: Womparable> {} // expected-error {{'where' clause next to generic parameters is obsolete}} {{12-32=}} {{33-33= where T: Womparable}}
