// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift

// Sized-by variant of the shared-count mixed-nullability tests. See the
// `CountedByOrNull/SharedCountMixedNullable.swift` file for design notes.

// Case 1: non-Optional sharer first.
@_SwiftifyImport(.sizedBy(pointer: .param(1), size: "size"), .sizedByOrNull(pointer: .param(2), size: "size"), nullableAsEmptySpan: true)
public func nonOptionalFirst(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ size: CInt) {
}

// Case 2: Optional sharer first.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(2), size: "size"), nullableAsEmptySpan: true)
public func optionalFirst(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ size: CInt) {
}

// Case 3: all sharers Optional.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedByOrNull(pointer: .param(2), size: "size"), nullableAsEmptySpan: true)
public func allOptional(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ size: CInt) {
}

// Case 4: three sharers, mixed.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(2), size: "size"), .sizedByOrNull(pointer: .param(3), size: "size"), nullableAsEmptySpan: true)
public func threeMixed(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ p3: UnsafeRawPointer?, _ size: CInt) {
}

// Case 5: three sharers, all Optional.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedByOrNull(pointer: .param(2), size: "size"), .sizedByOrNull(pointer: .param(3), size: "size"), nullableAsEmptySpan: true)
public func threeAllOptional(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ p3: UnsafeRawPointer?, _ size: CInt) {
}

// Case 6: parameter + return value sharing a size.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedBy(pointer: .return, size: "size"), nullableAsEmptySpan: true)
public func paramOptionalReturn(_ p1: UnsafeRawPointer?, _ size: CInt) -> UnsafeRawPointer? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeRawPointer?'}}
}

// Case 7: two parameters and a return sharing a size, mixed.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .sizedBy(pointer: .param(2), size: "size"), .sizedByOrNull(pointer: .return, size: "size"), nullableAsEmptySpan: true)
public func twoParamsAndReturn(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ size: CInt) -> UnsafeRawPointer? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeRawPointer?'}}
}

// Case 8: nonescaping (RawSpan) variants — verifies unsafe-prefix handling
// for Span vs UnsafeRawBufferPointer.
@_SwiftifyImport(.sizedByOrNull(pointer: .param(1), size: "size"), .nonescaping(pointer: .param(1)), .sizedByOrNull(pointer: .param(2), size: "size"), .nonescaping(pointer: .param(2)), nullableAsEmptySpan: true)
public func spansAllOptional(_ p1: UnsafeRawPointer?, _ p2: UnsafeRawPointer?, _ size: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test16nonOptionalFirst15_SwiftifyImportfMp_.swift
------------------------------
// Sized-by variant of the shared-count mixed-nullability tests. See the
// `CountedByOrNull/SharedCountMixedNullable.swift` file for design notes.

// Case 1: non-Optional sharer first.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonOptionalFirst(_ p1: UnsafeRawBufferPointer, _ p2: UnsafeRawBufferPointer?) {
    let size = CInt(exactly: p1.count)!
    if let _p2Count = unsafe p2?.count, _p2Count != size {
      fatalError("bounds check failure in nonOptionalFirst: expected \(size) but got \(_p2Count)")
    }
    return unsafe nonOptionalFirst(p1.baseAddress, p2?.baseAddress, size)
}
------------------------------
@__swiftmacro_4test13optionalFirst15_SwiftifyImportfMp_.swift
------------------------------
// Case 2: Optional sharer first.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func optionalFirst(_ p1: UnsafeRawBufferPointer?, _ p2: UnsafeRawBufferPointer) {
    let size = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != size {
      fatalError("bounds check failure in optionalFirst: expected \(size) but got \(_p1Count)")
    }
    return unsafe optionalFirst(p1?.baseAddress, p2.baseAddress, size)
}
------------------------------
@__swiftmacro_4test11allOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 3: all sharers Optional.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func allOptional(_ p1: UnsafeRawBufferPointer?, _ p2: UnsafeRawBufferPointer?) {
    let size = CInt(exactly: unsafe p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = unsafe p1?.count, _p1Count != size {
      fatalError("bounds check failure in allOptional: expected \(size) but got \(_p1Count)")
    }
    return unsafe allOptional(p1?.baseAddress, p2?.baseAddress, size)
}
------------------------------
@__swiftmacro_4test10threeMixed15_SwiftifyImportfMp_.swift
------------------------------
// Case 4: three sharers, mixed.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func threeMixed(_ p1: UnsafeRawBufferPointer?, _ p2: UnsafeRawBufferPointer, _ p3: UnsafeRawBufferPointer?) {
    let size = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != size {
      fatalError("bounds check failure in threeMixed: expected \(size) but got \(_p1Count)")
    }
    if let _p3Count = unsafe p3?.count, _p3Count != size {
      fatalError("bounds check failure in threeMixed: expected \(size) but got \(_p3Count)")
    }
    return unsafe threeMixed(p1?.baseAddress, p2.baseAddress, p3?.baseAddress, size)
}
------------------------------
@__swiftmacro_4test16threeAllOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 5: three sharers, all Optional.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func threeAllOptional(_ p1: UnsafeRawBufferPointer?, _ p2: UnsafeRawBufferPointer?, _ p3: UnsafeRawBufferPointer?) {
    let size = CInt(exactly: unsafe p3?.count ?? p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = unsafe p1?.count, _p1Count != size {
      fatalError("bounds check failure in threeAllOptional: expected \(size) but got \(_p1Count)")
    }
    if let _p2Count = unsafe p2?.count, _p2Count != size {
      fatalError("bounds check failure in threeAllOptional: expected \(size) but got \(_p2Count)")
    }
    return unsafe threeAllOptional(p1?.baseAddress, p2?.baseAddress, p3?.baseAddress, size)
}
------------------------------
@__swiftmacro_4test19paramOptionalReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 6: parameter + return value sharing a size.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func paramOptionalReturn(_ p1: UnsafeRawBufferPointer?) -> UnsafeRawBufferPointer {
    let size = CInt(exactly: unsafe p1?.count ?? 0)!
    return unsafe UnsafeRawBufferPointer(start: unsafe paramOptionalReturn(p1?.baseAddress, size), count: Int(size))
}
------------------------------
@__swiftmacro_4test18twoParamsAndReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 7: two parameters and a return sharing a size, mixed.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func twoParamsAndReturn(_ p1: UnsafeRawBufferPointer?, _ p2: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer? {
    let size = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != size {
      fatalError("bounds check failure in twoParamsAndReturn: expected \(size) but got \(_p1Count)")
    }
    let _resultValue = unsafe twoParamsAndReturn(p1?.baseAddress, p2.baseAddress, size)
    if unsafe _resultValue == nil {
      return nil
    }
    return unsafe UnsafeRawBufferPointer(start: _resultValue!, count: Int(size))
}
------------------------------
@__swiftmacro_4test16spansAllOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 8: nonescaping (RawSpan) variants — verifies unsafe-prefix handling
// for Span vs UnsafeRawBufferPointer.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func spansAllOptional(_ p1: RawSpan?, _ p2: RawSpan?) {
    let size = CInt(exactly: p2?.byteCount ?? p1?.byteCount ?? 0)!
    if let _p1Count = p1?.byteCount, _p1Count != size {
      fatalError("bounds check failure in spansAllOptional: expected \(size) but got \(_p1Count)")
    }
    let _p1Ptr = p1?.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeBytes {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe spansAllOptional(_p1Ptr?.baseAddress, _p2Ptr?.baseAddress, size)
}
------------------------------
