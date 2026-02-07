// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
public func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt) -> CInt {
// expected-error@+1{{missing return in global function expected to return 'CInt' (aka 'Int32')}}
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: RawSpan) -> CInt {
    let size = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    return unsafe myFunc(_ptrPtr.baseAddress!, size)
}
------------------------------
