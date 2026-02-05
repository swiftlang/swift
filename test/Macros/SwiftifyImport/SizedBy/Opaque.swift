// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
public func nonnullUnsafeRawBufferPointer(_ ptr: OpaquePointer, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
public func nullableUnsafeRawBufferPointer(_ ptr: OpaquePointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
public func impNullableUnsafeRawBufferPointer(_ ptr: OpaquePointer!, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
public func nonnullSpan(_ ptr: OpaquePointer, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
public func nullableSpan(_ ptr: OpaquePointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)))
public func impNullableSpan(_ ptr: OpaquePointer!, _ size: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test29nonnullUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonnullUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    return unsafe nonnullUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), size)
}
------------------------------
@__swiftmacro_4test30nullableUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer?) {
    let size = CInt(exactly: unsafe ptr?.count ?? 0)!
    return unsafe nullableUnsafeRawBufferPointer(OpaquePointer(ptr?.baseAddress), size)
}
------------------------------
@__swiftmacro_4test33impNullableUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func impNullableUnsafeRawBufferPointer(_ ptr: UnsafeRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    return unsafe impNullableUnsafeRawBufferPointer(OpaquePointer(ptr.baseAddress!), size)
}
------------------------------
@__swiftmacro_4test11nonnullSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonnullSpan(_ ptr: RawSpan) {
    let size = CInt(exactly: ptr.byteCount)!
    return unsafe ptr.withUnsafeBytes { _ptrPtr in
      return unsafe nonnullSpan(OpaquePointer(_ptrPtr.baseAddress!), size)
    }
}
------------------------------
@__swiftmacro_4test12nullableSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nullableSpan(_ ptr: RawSpan?) {
    let size = CInt(exactly: ptr?.byteCount ?? 0)!
    return { () in
        return if ptr == nil {
            unsafe nullableSpan(nil, size)
          } else {
            unsafe ptr!.withUnsafeBytes { _ptrPtr in
              return unsafe nullableSpan(OpaquePointer(_ptrPtr.baseAddress), size)
            }
          }
    }()
}
------------------------------
@__swiftmacro_4test15impNullableSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func impNullableSpan(_ ptr: RawSpan) {
    let size = CInt(exactly: ptr.byteCount)!
    return unsafe ptr.withUnsafeBytes { _ptrPtr in
      return unsafe impNullableSpan(OpaquePointer(_ptrPtr.baseAddress!), size)
    }
}
------------------------------
