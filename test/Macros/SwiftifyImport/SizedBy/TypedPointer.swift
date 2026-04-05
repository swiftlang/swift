// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature LifetimeDependence -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature LifetimeDependence 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
public func constParam(_ ptr: UnsafePointer<CChar>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"))
public func mutParam(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size * count"))
public func exprParam(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt, _ count: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
public func constReturn(_ size: CInt) -> UnsafePointer<CChar> { return unsafe UnsafePointer(OpaquePointer(bitPattern: 0)!) }

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
public func mutReturn(_ size: CInt) -> UnsafeMutablePointer<UInt8> { return unsafe UnsafeMutablePointer(OpaquePointer(bitPattern: 0)!) }

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size * count"))
public func exprReturn(_ size: CInt, _ count: CInt) -> UnsafeMutablePointer<UInt8> { return unsafe UnsafeMutablePointer(OpaquePointer(bitPattern: 0)!) }

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"),
                 .nonescaping(pointer: .param(1)))
public func constParamNoreturn(_ ptr: UnsafePointer<CChar>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"),
                 .nonescaping(pointer: .param(1)))
public func mutParamNoreturn(_ ptr: UnsafeMutablePointer<UInt8>, _ size: CInt) {}

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "size"),
                 .sizedBy(pointer: .return, size: "size"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
public func constReturnDependence(_ size: CInt, _ ptr: UnsafePointer<UInt8>) -> UnsafePointer<CChar> { return unsafe UnsafePointer(OpaquePointer(bitPattern: 0)!) }

@_SwiftifyImport(.sizedBy(pointer: .param(2), size: "size"),
                 .sizedBy(pointer: .return, size: "size"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
public func mutReturnDependence(_ size: CInt, _ ptr: UnsafeMutablePointer<UInt8>) -> UnsafeMutablePointer<UInt8> { return unsafe UnsafeMutablePointer(OpaquePointer(bitPattern: 0)!) }

//--- expansions.expected
@__swiftmacro_4test10constParam15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func constParam(_ ptr: UnsafeRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    return unsafe constParam(ptr.baseAddress!.assumingMemoryBound(to: CChar.self), size)
}
------------------------------
@__swiftmacro_4test8mutParam15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mutParam(_ ptr: UnsafeMutableRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    return unsafe mutParam(ptr.baseAddress!.assumingMemoryBound(to: UInt8.self), size)
}
------------------------------
@__swiftmacro_4test9exprParam15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func exprParam(_ ptr: UnsafeMutableRawBufferPointer, _ size: CInt, _ count: CInt) {
    let _ptrCount = ptr.count
    if _ptrCount != size * count {
      fatalError("bounds check failure in exprParam: expected \(size * count) but got \(_ptrCount)")
    }
    return unsafe exprParam(ptr.baseAddress!.assumingMemoryBound(to: UInt8.self), size, count)
}
------------------------------
@__swiftmacro_4test11constReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func constReturn(_ size: CInt) -> UnsafeRawBufferPointer {
    return unsafe UnsafeRawBufferPointer(start: unsafe constReturn(size), count: Int(size))
}
------------------------------
@__swiftmacro_4test9mutReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mutReturn(_ size: CInt) -> UnsafeMutableRawBufferPointer {
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe mutReturn(size), count: Int(size))
}
------------------------------
@__swiftmacro_4test10exprReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func exprReturn(_ size: CInt, _ count: CInt) -> UnsafeMutableRawBufferPointer {
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe exprReturn(size, count), count: Int(size * count))
}
------------------------------
@__swiftmacro_4test18constParamNoreturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func constParamNoreturn(_ ptr: RawSpan) {
    let size = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe constParamNoreturn(_ptrPtr.baseAddress!.assumingMemoryBound(to: CChar.self), size)
}
------------------------------
@__swiftmacro_4test16mutParamNoreturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func mutParamNoreturn(_ ptr: inout MutableRawSpan) {
    let size = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = unsafe ptr.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe mutParamNoreturn(_ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self), size)
}
------------------------------
@__swiftmacro_4test21constReturnDependence15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_disfavoredOverload
public func constReturnDependence(_ ptr: RawSpan) -> RawSpan {
    let size = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = unsafe ptr.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe constReturnDependence(size, _ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self)), byteCount: Int(size)), copying: ())
}
------------------------------
@__swiftmacro_4test19mutReturnDependence15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func mutReturnDependence(_ ptr: inout MutableRawSpan) -> MutableRawSpan {
    let size = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = unsafe ptr.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe mutReturnDependence(size, _ptrPtr.baseAddress!.assumingMemoryBound(to: UInt8.self)), byteCount: Int(size)), copying: ())
}
------------------------------
