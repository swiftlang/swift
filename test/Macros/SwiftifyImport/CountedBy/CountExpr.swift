// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "size * count"))
public func myFunc(_ ptr: UnsafePointer<CInt>, _ size: CInt, _ count: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ size: CInt, _ count: CInt) {
    let _ptrCount = ptr.count
    if _ptrCount != size * count {
      fatalError("bounds check failure in myFunc: expected \(size * count) but got \(_ptrCount)")
    }
    return unsafe myFunc(ptr.baseAddress!, size, count)
}
------------------------------
