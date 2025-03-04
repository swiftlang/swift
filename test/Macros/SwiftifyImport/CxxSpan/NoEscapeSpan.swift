// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-frontend %s -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature LifetimeDependence -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

import CxxStdlib
import StdSpan

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc(_ span: SpanOfInt, _ secondSpan: SpanOfInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
// CHECK-NEXT:     return unsafe myFunc(SpanOfInt(span), secondSpan)
// CHECK-NEXT: }
