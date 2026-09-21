// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), nullableAsEmptySpan: true)
public func myFunc(_ ptr: UnsafeRawPointer?, _ size: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .nonescaping(pointer: .param(1)), nullableAsEmptySpan: true)
public func myFunc2(_ ptr: UnsafeMutableRawPointer?, _ len: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .nonescaping(pointer: .param(1)), .sizedBy(pointer: .param(3), size: "len2"), .nonescaping(pointer: .param(3)), nullableAsEmptySpan: true)
public func myFunc3(_ ptr: UnsafeMutableRawPointer?, _ len: CInt, _ ptr2: UnsafeMutableRawPointer?, _ len2: CInt) {
}

@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "len"), .sizedBy(pointer: .return, size: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), nullableAsEmptySpan: true)
public func myFunc4(_ ptr: UnsafeMutableRawPointer?, _ len: CInt) -> UnsafeMutableRawPointer? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutableRawPointer?'}}
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: UnsafeRawBufferPointer) {
    let size = CInt(exactly: ptr.count)!
    return unsafe myFunc(ptr.baseAddress, size)
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func myFunc2(_ ptr: inout MutableRawSpan) {
    let len = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = ptr.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe myFunc2(_ptrPtr.baseAddress, len)
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_lifetime(ptr2: copy ptr2) @_disfavoredOverload
public func myFunc3(_ ptr: inout MutableRawSpan, _ ptr2: inout MutableRawSpan) {
    let len = CInt(exactly: ptr.byteCount)!
    let len2 = CInt(exactly: ptr2.byteCount)!
    let _ptrPtr = ptr.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    let _ptr2Ptr = ptr2.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr2)
    }
    return unsafe myFunc3(_ptrPtr.baseAddress, len, _ptr2Ptr.baseAddress, len2)
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func myFunc4(_ ptr: inout MutableRawSpan) -> MutableRawSpan {
    let len = CInt(exactly: ptr.byteCount)!
    let _ptrPtr = ptr.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    let _resultValue = unsafe myFunc4(_ptrPtr.baseAddress, len)
    if unsafe _resultValue == nil {
      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")
      return MutableRawSpan()
    }
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())
}
------------------------------
