
// REQUIRES: swift_swift_parser

// RUN: %target-swift-frontend %s -enable-experimental-cxx-interop -I %S/Inputs -Xcc -std=c++20 -swift-version 5 -module-name main -disable-availability-checking -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -verify -strict-memory-safety 2>&1 | %FileCheck --match-full-lines %s

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu

import CxxStdlib
import StdSpan

public struct VecOfInt {}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.span<__cxxConst<CInt>>"])
func myFunc(_ span: SpanOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc2(_ vec: borrowing VecOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc3(_ span1: SpanOfInt, _ span2: SpanOfInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc4(_ vec: borrowing VecOfInt, _ span: SpanOfInt) -> SpanOfInt {
}

struct X {
  @_SwiftifyImport(.lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
  func myFunc5() -> SpanOfInt {}
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy),
                 .sizedBy(pointer: .param(2), size: "count * size"),
                 .nonescaping(pointer: .param(2)),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc6(_ span: SpanOfInt, _ ptr: UnsafeRawPointer, _ count: CInt, _ size: CInt) -> SpanOfInt {
}

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "count * size"),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy),
                 .nonescaping(pointer: .param(2)),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc7(_ span: SpanOfInt, _ ptr: UnsafeRawPointer, _ count: CInt, _ size: CInt) -> SpanOfInt {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "count * size"),
                 .nonescaping(pointer: .param(1)),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
func myFunc8(_ ptr: UnsafeRawPointer, _ span: SpanOfInt, _ count: CInt, _ size: CInt) -> SpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
func myFunc9(_ span: MutableSpanOfInt) -> MutableSpanOfInt {
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
func myFunc10(_ self: MutableSpanOfInt) -> MutableSpanOfInt {
}

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc(_ span: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc(SpanOfInt(span))), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(borrow vec) @_disfavoredOverload
// CHECK-NEXT: func myFunc2(_ vec: borrowing VecOfInt) -> Span<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc2(vec)), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span1, copy span2) @_disfavoredOverload
// CHECK-NEXT: func myFunc3(_ span1: Span<CInt>, _ span2: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc3(SpanOfInt(span1), SpanOfInt(span2))), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(borrow vec, copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc4(_ vec: borrowing VecOfInt, _ span: Span<CInt>) -> Span<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc4(vec, SpanOfInt(span))), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(borrow self) @_disfavoredOverload
// CHECK-NEXT: func myFunc5() -> Span<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc5()), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc6(_ span: Span<CInt>, _ ptr: RawSpan, _ count: CInt, _ size: CInt) -> Span<CInt> {
// CHECK-NEXT:     let _ptrCount = ptr.byteCount
// CHECK-NEXT:     if _ptrCount != count * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc6: expected \(count * size) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe myFunc6(SpanOfInt(span), _ptrPtr.baseAddress!, count, size)
// CHECK-NEXT:             }), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc7(_ span: Span<CInt>, _ ptr: RawSpan, _ count: CInt, _ size: CInt) -> Span<CInt> {
// CHECK-NEXT:     let _ptrCount = ptr.byteCount
// CHECK-NEXT:     if _ptrCount != count * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc7: expected \(count * size) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe myFunc7(SpanOfInt(span), _ptrPtr.baseAddress!, count, size)
// CHECK-NEXT:             }), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc8(_ ptr: RawSpan, _ span: Span<CInt>, _ count: CInt, _ size: CInt) -> Span<CInt> {
// CHECK-NEXT:     let _ptrCount = ptr.byteCount
// CHECK-NEXT:     if _ptrCount != count * size {
// CHECK-NEXT:       fatalError("bounds check failure in myFunc8: expected \(count * size) but got \(_ptrCount)")
// CHECK-NEXT:     }
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe ptr.withUnsafeBytes { _ptrPtr in
// CHECK-NEXT:       return unsafe myFunc8(_ptrPtr.baseAddress!, SpanOfInt(span), count, size)
// CHECK-NEXT:             }), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy span) @_lifetime(span: copy span) @_disfavoredOverload
// CHECK-NEXT: func myFunc9(_ span: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
// CHECK-NEXT:       return unsafe myFunc9(MutableSpanOfInt(_spanPtr))
// CHECK-NEXT:             }), copying: ())
// CHECK-NEXT: }

// CHECK:      @_alwaysEmitIntoClient @_lifetime(copy `self`) @_lifetime(`self`: copy `self`) @_disfavoredOverload
// CHECK-NEXT: func myFunc10(_ `self`: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
// CHECK-NEXT:     return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe `self`.withUnsafeMutableBufferPointer { _selfPtr in
// CHECK-NEXT:         return unsafe myFunc10(MutableSpanOfInt(_selfPtr))
// CHECK-NEXT:    }), copying: ())
// CHECK-NEXT: }
