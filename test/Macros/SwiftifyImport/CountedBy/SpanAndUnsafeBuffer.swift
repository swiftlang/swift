// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len1"), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(1)))
public func myFunc(_ ptr1: UnsafePointer<CInt>, _ len1: CInt, _ ptr2: UnsafePointer<CInt>, _ len2: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr1: Span<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
    let len1 = CInt(exactly: ptr1.count)!
    let len2 = CInt(exactly: ptr2.count)!
    return unsafe ptr1.withUnsafeBufferPointer { _ptr1Ptr in
      return unsafe myFunc(_ptr1Ptr.baseAddress!, len1, ptr2.baseAddress!, len2)
    }
}
------------------------------
