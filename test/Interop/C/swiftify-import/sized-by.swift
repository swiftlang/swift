// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:SizedByClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers %t/test.swift 2> %t/expansions.out
// RUN: diff --strip-trailing-cr %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe simple(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So9swiftAttr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe swiftAttr(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
    let len = Int32(exactly: unsafe p1.count)!
    if unsafe p2.count != len {
      fatalError("bounds check failure in shared: expected \(len) but got \(unsafe p2.count)")
    }
    return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableRawBufferPointer) {
    let _pCount = unsafe p.count
    if _pCount != len - offset {
      fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
    }
    return unsafe complexExpr(len, offset, p.baseAddress!)
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe nullUnspecified(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe nonnull(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableRawBufferPointer?) {
    let len = Int32(exactly: unsafe p?.count ?? 0)!
    return unsafe nullable(len, p?.baseAddress)
}
------------------------------
@__swiftmacro_So13returnPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableRawBufferPointer {
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe returnPointer(len), count: Int(len))
}
------------------------------
@__swiftmacro_So6opaque15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func opaque(_ p: UnsafeRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe opaque(len, OpaquePointer(p.baseAddress!))
}
------------------------------
@__swiftmacro_So9opaqueptr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func opaqueptr(_ p: UnsafeRawBufferPointer) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe opaqueptr(len, OpaquePointer(p.baseAddress!))
}
------------------------------
@__swiftmacro_So9charsized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func charsized(_ _charsized_param0: UnsafeMutableRawBufferPointer) {
    let _charsized_param1 = Int32(exactly: unsafe _charsized_param0.count)!
    return unsafe charsized(_charsized_param0.baseAddress!.assumingMemoryBound(to: CChar.self), _charsized_param1)
}
------------------------------
@__swiftmacro_So9bytesized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func bytesized(_ size: Int32) -> UnsafeMutableRawBufferPointer {
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe bytesized(size), count: Int(size))
}
------------------------------
@__swiftmacro_So16aliasedBytesized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func aliasedBytesized(_ p: UnsafeMutableRawBufferPointer) {
    let size = Int32(exactly: unsafe p.count)!
    return unsafe aliasedBytesized(p.baseAddress!.assumingMemoryBound(to: UInt8.self), size)
}
------------------------------
