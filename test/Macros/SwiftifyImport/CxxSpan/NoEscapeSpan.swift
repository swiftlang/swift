// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

public struct SpanOfInt {
    init(_ x: Span<CInt>) {}
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["SpanOfInt" : "std.span<CInt>"])
func myFunc(_ span: SpanOfInt, _ secondSpan: SpanOfInt) {
}

// CHECK:      @_alwaysEmitIntoClient
// CHECK-NEXT: func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
// CHECK-NEXT:     return myFunc(SpanOfInt(span), secondSpan)
// CHECK-NEXT: }
