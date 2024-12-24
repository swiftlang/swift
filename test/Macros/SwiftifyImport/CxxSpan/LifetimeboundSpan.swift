
// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Span

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature Span  -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

public struct SpanOfInt {
    init(_ x: Span<CInt>) {}
}

public struct VecOfInt {}

@_SwiftifyImport(.lifetimeDependence(from: 1, to: 0, type: "copy"), typeMappings: ["SpanOfInt" : "span<CInt>"])
func myFunc(_ span: SpanOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(from: 1, to: 0, type: "borrow"), typeMappings: ["SpanOfInt" : "span<CInt>"])
func myFunc2(_ vec: borrowing VecOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(from: 1, to: 2, type: "copy"), typeMappings: ["SpanOfInt" : "span<CInt>"])
func myFunc3(_ inp: SpanOfInt, _ out: inout SpanOfInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
// CHECK-NEXT:     return myFunc(SpanOfInt(span), secondSpan)
// CHECK-NEXT: }
