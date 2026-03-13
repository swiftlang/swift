// REQUIRES: swift_swift_parser
// REQUIRES: std_span

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
import CxxStdlib
import StdSpan

public struct VecOfInt {}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.span<__cxxConst<CInt>>"])
public func myFunc(_ span: SpanOfInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc2(_ vec: borrowing VecOfInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc3(_ span1: SpanOfInt, _ span2: SpanOfInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc4(_ vec: borrowing VecOfInt, _ span: SpanOfInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

struct X {
  @_SwiftifyImport(.lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow), typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
  public func myFunc5() -> SpanOfInt { unsafe SpanOfInt() }
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy),
                 .sizedBy(pointer: .param(2), size: "count * size"),
                 .nonescaping(pointer: .param(2)),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc6(_ span: SpanOfInt, _ ptr: UnsafeRawPointer, _ count: CInt, _ size: CInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "count * size"),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy),
                 .nonescaping(pointer: .param(2)),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc7(_ span: SpanOfInt, _ ptr: UnsafeRawPointer, _ count: CInt, _ size: CInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "count * size"),
                 .nonescaping(pointer: .param(1)),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy),
                 typeMappings: ["SpanOfInt" : "std.__1.span<__cxxConst<CInt>, _CUnsignedLong_18446744073709551615>"])
public func myFunc8(_ ptr: UnsafeRawPointer, _ span: SpanOfInt, _ count: CInt, _ size: CInt) -> SpanOfInt {
  unsafe SpanOfInt()
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
public func myFunc9(_ span: MutableSpanOfInt) -> MutableSpanOfInt {
  unsafe MutableSpanOfInt()
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
public func myFunc10(_ self: MutableSpanOfInt) -> MutableSpanOfInt {
  unsafe MutableSpanOfInt()
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
public func myFunc(_ span: Span<CInt>) -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc(SpanOfInt(span))), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow vec) @_disfavoredOverload
public func myFunc2(_ vec: borrowing VecOfInt) -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc2(vec)), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span1, copy span2) @_disfavoredOverload
public func myFunc3(_ span1: Span<CInt>, _ span2: Span<CInt>) -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc3(SpanOfInt(span1), SpanOfInt(span2))), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow vec, copy span) @_disfavoredOverload
public func myFunc4(_ vec: borrowing VecOfInt, _ span: Span<CInt>) -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc4(vec, SpanOfInt(span))), copying: ())
}
------------------------------
@__swiftmacro_4test1XV7myFunc515_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
  @_alwaysEmitIntoClient @_lifetime(borrow self) @_disfavoredOverload
  public func myFunc5() -> Span<CInt> {
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc5()), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc615_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
public func myFunc6(_ span: Span<CInt>, _ ptr: RawSpan, _ count: CInt, _ size: CInt) -> Span<CInt> {
    let _ptrCount = ptr.byteCount
    if _ptrCount != count * size {
      fatalError("bounds check failure in myFunc6: expected \(count * size) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc6(SpanOfInt(span), _ptrPtr.baseAddress!, count, size)), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc715_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
public func myFunc7(_ span: Span<CInt>, _ ptr: RawSpan, _ count: CInt, _ size: CInt) -> Span<CInt> {
    let _ptrCount = ptr.byteCount
    if _ptrCount != count * size {
      fatalError("bounds check failure in myFunc7: expected \(count * size) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc7(SpanOfInt(span), _ptrPtr.baseAddress!, count, size)), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc815_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span) @_disfavoredOverload
public func myFunc8(_ ptr: RawSpan, _ span: Span<CInt>, _ count: CInt, _ size: CInt) -> Span<CInt> {
    let _ptrCount = ptr.byteCount
    if _ptrCount != count * size {
      fatalError("bounds check failure in myFunc8: expected \(count * size) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(Span(_unsafeCxxSpan: unsafe myFunc8(_ptrPtr.baseAddress!, SpanOfInt(span), count, size)), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc915_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy span) @_lifetime(span: copy span) @_disfavoredOverload
public func myFunc9(_ span: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
      return unsafe myFunc9(MutableSpanOfInt(_spanPtr))
            }), copying: ())
}
------------------------------
@__swiftmacro_4test8myFunc1015_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy `self`) @_lifetime(`self`: copy `self`) @_disfavoredOverload
public func myFunc10(_ `self`: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan(_unsafeCxxSpan: unsafe `self`.withUnsafeMutableBufferPointer { _selfPtr in
      return unsafe myFunc10(MutableSpanOfInt(_selfPtr))
            }), copying: ())
}
------------------------------
