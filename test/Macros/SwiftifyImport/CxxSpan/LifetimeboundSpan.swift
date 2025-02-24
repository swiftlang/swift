
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %target-swift-frontend %s -enable-experimental-cxx-interop -I %S/Inputs -Xcc -std=c++20 -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature LifetimeDependence -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

import CxxStdlib
import StdSpan

public struct VecOfInt {}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc(_ span: SpanOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc2(_ vec: borrowing VecOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc3(_ span1: SpanOfInt, _ span2: SpanOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc4(_ vec: borrowing VecOfInt, _ span: SpanOfInt) -> SpanOfInt {
}

struct X {
  @_SwiftifyImport(.lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
  func myFunc5() -> SpanOfInt {}
}

// CHECK:      @_alwaysEmitIntoClient @lifetime(span)
// CHECK-NEXT: func myFunc(_ span: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return _unsafeRemoveLifetime(Span(_unsafeCxxSpan: myFunc(SpanOfInt(span))))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow vec) @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ vec: borrowing VecOfInt) -> Span<CInt> {
// CHECK-NEXT:     return _unsafeRemoveLifetime(Span(_unsafeCxxSpan: myFunc2(vec)))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(span1, span2)
// CHECK-NEXT: func myFunc3(_ span1: Span<CInt>, _ span2: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return _unsafeRemoveLifetime(Span(_unsafeCxxSpan: myFunc3(SpanOfInt(span1), SpanOfInt(span2))))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow vec, span)
// CHECK-NEXT: func myFunc4(_ vec: borrowing VecOfInt, _ span: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return _unsafeRemoveLifetime(Span(_unsafeCxxSpan: myFunc4(vec, SpanOfInt(span))))
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT: func myFunc5() -> Span<CInt> {
// CHECK-NEXT:     return _unsafeRemoveLifetime(Span(_unsafeCxxSpan: myFunc5()))
// CHECK-NEXT: }
