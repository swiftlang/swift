
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -enable-sil-ownership -swift-version 3 -verify | %FileCheck %s

protocol Foo {}

// Don't crash mangling single-protocol "composition" types.
// CHECK-LABEL: sil hidden @$S15mangling_swift327single_protocol_composition1xyAA3Foo_p_tF
func single_protocol_composition(x: protocol<Foo>) {} // expected-warning {{'protocol<...>' composition syntax is deprecated and not needed here}}
