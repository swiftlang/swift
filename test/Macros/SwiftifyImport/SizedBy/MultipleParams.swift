// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(3), size: "size2"))
public func myFunc(_ ptr: UnsafeRawPointer, _ size: CInt, _ ptr2: UnsafeRawPointer, _ size2: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: UnsafeRawBufferPointer, _ ptr2: UnsafeRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    let size2 = CInt(exactly: ptr2.count)!
    return unsafe myFunc(ptr.baseAddress!, size, ptr2.baseAddress!, size2)
}
------------------------------
