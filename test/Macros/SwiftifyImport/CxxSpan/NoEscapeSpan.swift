// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: std_span

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -cxx-interoperability-mode=default -I %S/Inputs -Xcc -std=c++20 -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Lifetimes 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
import CxxStdlib
import StdSpan

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["SpanOfInt" : "std.span<__cxxConst<CInt>>"])
public func myFunc(_ span: SpanOfInt, _ secondSpan: SpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
public func myFunc2(_ span: MutableSpanOfInt, _ secondSpan: MutableSpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), .nonescaping(pointer: .param(2)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>", "SpanOfInt" : "std.span<__cxxConst<CInt>>"])
public func myFunc3(_ span: MutableSpanOfInt, _ secondSpan: SpanOfInt) {
}

@_SwiftifyImport(.nonescaping(pointer: .param(1)), .nonescaping(pointer: .param(2)), typeMappings: ["MutableSpanOfInt" : "std.span<CInt>"])
public func myFunc4(_ span: MutableSpanOfInt, _ secondSpan: MutableSpanOfInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ span: Span<CInt>, _ secondSpan: SpanOfInt) {
    return unsafe myFunc(SpanOfInt(span), secondSpan)
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(span: copy span) @_disfavoredOverload
public func myFunc2(_ span: inout MutableSpan<CInt>, _ secondSpan: MutableSpanOfInt) {
    return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
      return unsafe myFunc2(MutableSpanOfInt(_spanPtr), secondSpan)
    }
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(span: copy span) @_disfavoredOverload
public func myFunc3(_ span: inout MutableSpan<CInt>, _ secondSpan: Span<CInt>) {
    return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
      return unsafe myFunc3(MutableSpanOfInt(_spanPtr), SpanOfInt(secondSpan))
    }
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(span: copy span) @_lifetime(secondSpan: copy secondSpan) @_disfavoredOverload
public func myFunc4(_ span: inout MutableSpan<CInt>, _ secondSpan: inout MutableSpan<CInt>) {
    return unsafe secondSpan.withUnsafeMutableBufferPointer { _secondSpanPtr in
      return unsafe span.withUnsafeMutableBufferPointer { _spanPtr in
      return unsafe myFunc4(MutableSpanOfInt(_spanPtr), MutableSpanOfInt(_secondSpanPtr))
      }
    }
}
------------------------------
