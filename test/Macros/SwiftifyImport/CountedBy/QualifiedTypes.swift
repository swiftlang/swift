// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func foo(_ ptr: Swift.UnsafePointer<Swift.Int>, _ len: Swift.Int) -> Swift.Void {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func bar(_ ptr: Swift.UnsafePointer<Swift.CInt>, _ len: Swift.Int) -> () {
}

//--- expansions.expected
@__swiftmacro_4test3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func foo(_ ptr: Swift.UnsafeBufferPointer<Swift.Int>) -> Swift.Void {
    let len = ptr.count
    return unsafe foo(ptr.baseAddress!, len)
}
------------------------------
@__swiftmacro_4test3bar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func bar(_ ptr: Swift.UnsafeBufferPointer<Swift.CInt>) -> () {
    let len = ptr.count
    return unsafe bar(ptr.baseAddress!, len)
}
------------------------------
