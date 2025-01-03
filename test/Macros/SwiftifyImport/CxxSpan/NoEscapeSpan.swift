// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Span

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature Span  -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

public struct SpanOfInt {
    init(_ x: Span<CInt>) {}
}

@_SwiftifyImport(.nonescaping(pointer: 1), typeMappings: ["SpanOfInt" : "span<CInt>"])
func myFunc(_ span: SpanOfInt, _ secondSpan: SpanOfInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
// CHECK-NEXT:     return myFunc(SpanOfInt(span), secondSpan)
// CHECK-NEXT: }
