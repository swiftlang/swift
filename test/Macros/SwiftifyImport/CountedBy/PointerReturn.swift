// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions -enable-experimental-feature Lifetimes 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .return, count: "len"))
public func myFunc(_ len: CInt) -> UnsafeMutablePointer<CInt> {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .nonescaping(pointer: .return))
public func nonEscaping(_ len: CInt) -> UnsafePointer<CInt> {
// expected-error@+1{{missing return in global function expected to return 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len2"), .countedBy(pointer: .param(1), count: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func lifetimeDependentCopy(_ p: UnsafePointer<CInt>, _ len1: CInt, _ len2: CInt) -> UnsafePointer<CInt> {
// expected-error@+1{{missing return in global function expected to return 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len2"), .countedBy(pointer: .param(1), count: "len1"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
public func lifetimeDependentBorrow(_ p: borrowing UnsafePointer<CInt>, _ len1: CInt, _ len2: CInt) -> UnsafePointer<CInt> {
// expected-error@+1{{missing return in global function expected to return 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
    return unsafe UnsafeMutableBufferPointer<CInt> (start: unsafe myFunc(len), count: Int(len))
}
------------------------------
@__swiftmacro_4test11nonEscaping15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonEscaping(_ len: CInt) -> UnsafeBufferPointer<CInt> {
    return unsafe UnsafeBufferPointer<CInt> (start: unsafe nonEscaping(len), count: Int(len))
}
------------------------------
@__swiftmacro_4test21lifetimeDependentCopy15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
public func lifetimeDependentCopy(_ p: Span<CInt>, _ len2: CInt) -> Span<CInt> {
    let len1 = CInt(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(Span<CInt> (_unsafeStart: unsafe p.withUnsafeBufferPointer { _pPtr in
      return unsafe lifetimeDependentCopy(_pPtr.baseAddress!, len1, len2)
            }, count: Int(len2)), copying: ())
}
------------------------------
@__swiftmacro_4test23lifetimeDependentBorrow15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(borrow p) @_disfavoredOverload
public func lifetimeDependentBorrow(_ p: borrowing UnsafeBufferPointer<CInt>, _ len2: CInt) -> Span<CInt> {
    let len1 = CInt(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(Span<CInt> (_unsafeStart: unsafe lifetimeDependentBorrow(p.baseAddress!, len1, len2), count: Int(len2)), copying: ())
}
------------------------------
