// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %target-swift-frontend %s -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -swift-version 5 -module-name main -disable-availability-checking -typecheck -enable-experimental-feature Lifetimes -plugin-path %swift-plugin-dir -strict-memory-safety -warnings-as-errors -dump-macro-expansions 2>&1 | %FileCheck --match-full-lines %s

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

import CxxStdlib
import StdSpan

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["SpanOfInt" : "std.span<__cxxConst<CInt>>"])
func myFunc(_ span: SpanOfInt, _ secondSpan: SpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
func myFunc2(_ span: MutableSpanOfInt, _ secondSpan: MutableSpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), .nonescaping(pointer: .param(2)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>", "SpanOfInt" : "std.span<__cxxConst<CInt>>"])
func myFunc3(_ span: MutableSpanOfInt, _ secondSpan: SpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), .nonescaping(pointer: .param(2)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
func myFunc4(_ span: MutableSpanOfInt, _ secondSpan: MutableSpanOfInt) {
}

// CHECK:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
// CHECK-NEXT:     return unsafe myFunc(SpanOfInt(span), secondSpan)
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(span: copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ span: inout MutableSpan<CInt>, _ secondSpan: MutableSpanOfInt) {
// CHECK-NEXT:     return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
// CHECK-NEXT:         return unsafe myFunc2(MutableSpanOfInt(_spanPtr), secondSpan)
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(span: copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc3(_ span: inout MutableSpan<CInt>, _ secondSpan: Span<CInt>) {
// CHECK-NEXT:     return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
// CHECK-NEXT:         return unsafe myFunc3(MutableSpanOfInt(_spanPtr), SpanOfInt(secondSpan))
// CHECK-NEXT:     }
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(span: copy span) @_lifetime(secondSpan: copy secondSpan) @_disfavoredOverload
// CHECK-NEXT: func myFunc4(_ span: inout MutableSpan<CInt>, _ secondSpan: inout MutableSpan<CInt>) {
// CHECK-NEXT:     return unsafe secondSpan.withUnsafeMutableBufferPointer { _secondSpanPtr in
// CHECK-NEXT:         return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
// CHECK-NEXT:             return unsafe myFunc4(MutableSpanOfInt(_spanPtr), MutableSpanOfInt(_secondSpanPtr))
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT: }
