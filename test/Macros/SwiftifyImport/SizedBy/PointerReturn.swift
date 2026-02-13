// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .return, size: "len"))
public func myFunc(_ len: CInt) -> UnsafeMutableRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutableRawPointer'}}
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len"), .nonescaping(pointer: .return))
public func nonEscaping(_ len: CInt) -> UnsafeRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeRawPointer'}}
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func lifetimeDependentCopy(_ p: UnsafeRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeRawPointer'}}
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
public func lifetimeDependentBorrow(_ p: borrowing UnsafeRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeRawPointer'}}
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func lifetimeDependentCopyMut(_ p: UnsafeMutableRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeMutableRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutableRawPointer'}}
}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "len2"), .sizedBy(pointer: .param(1), size: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
public func lifetimeDependentBorrowMut(_ p: borrowing UnsafeMutableRawPointer, _ len1: CInt, _ len2: CInt) -> UnsafeMutableRawPointer {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutableRawPointer'}}
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ len: CInt) -> UnsafeMutableRawBufferPointer {
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe myFunc(len), count: Int(len))
}
------------------------------
@__swiftmacro_4test11nonEscaping15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonEscaping(_ len: CInt) -> UnsafeRawBufferPointer {
    return unsafe UnsafeRawBufferPointer(start: unsafe nonEscaping(len), count: Int(len))
}
------------------------------
@__swiftmacro_4test21lifetimeDependentCopy15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
public func lifetimeDependentCopy(_ p: RawSpan, _ len2: CInt) -> RawSpan {
    let len1 = CInt(exactly: p.byteCount)!
    let _pPtr = unsafe p.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(p)
    }
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe lifetimeDependentCopy(_pPtr.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
}
------------------------------
@__swiftmacro_4test23lifetimeDependentBorrow15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow p) @_disfavoredOverload
public func lifetimeDependentBorrow(_ p: borrowing UnsafeRawBufferPointer, _ len2: CInt) -> RawSpan {
    let len1 = CInt(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe lifetimeDependentBorrow(p.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
}
------------------------------
@__swiftmacro_4test24lifetimeDependentCopyMut15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload
public func lifetimeDependentCopyMut(_ p: inout MutableRawSpan, _ len2: CInt) -> MutableRawSpan {
    let len1 = CInt(exactly: p.byteCount)!
    let _pPtr = unsafe p.withUnsafeMutableBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(p)
    }
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe lifetimeDependentCopyMut(_pPtr.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
}
------------------------------
@__swiftmacro_4test26lifetimeDependentBorrowMut15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow p) @_disfavoredOverload
public func lifetimeDependentBorrowMut(_ p: borrowing UnsafeMutableRawBufferPointer, _ len2: CInt) -> MutableRawSpan {
    let len1 = CInt(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe lifetimeDependentBorrowMut(p.baseAddress!, len1, len2), byteCount: Int(len2)), copying: ())
}
------------------------------
