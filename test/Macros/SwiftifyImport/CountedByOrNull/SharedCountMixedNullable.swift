// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift

// Mix of `.countedBy` (hides nullability — wrapper is non-Optional) and
// `.countedByOrNull` (preserves nullability — wrapper is Optional) sharing
// a count. The non-Optional sharer should be the count extractor regardless
// of declaration order, and the Optional sharer's check should skip when
// it is nil.

// Case 1: non-Optional sharer is first in declaration order.
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedByOrNull(pointer: .param(2), count: "len"))
public func nonOptionalFirst(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 2: Optional sharer is first in declaration order. Generated code
// should bind `len` from `p2` (the non-Optional one) anyway.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len"))
public func optionalFirst(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 3: all sharers Optional — `??`-chain extraction, nil-aware checks on
// every non-extractor sharer.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedByOrNull(pointer: .param(2), count: "len"))
public func allOptional(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 4: three sharers with mixed nullability.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len"), .countedByOrNull(pointer: .param(3), count: "len"))
public func threeMixed(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ p3: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 5: three sharers, all Optional — `??`-chain across all three.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedByOrNull(pointer: .param(2), count: "len"), .countedByOrNull(pointer: .param(3), count: "len"))
public func threeAllOptional(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ p3: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 6: parameter + return value sharing a count, mixed nullability.
// Return uses `len`; parameter extracts it.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedBy(pointer: .return, count: "len"))
public func paramOptionalReturn(_ p1: UnsafePointer<CInt>?, _ len: CInt) -> UnsafePointer<CInt>? {
// expected-error@+1{{missing return in global function expected to return 'UnsafePointer<CInt>?' (aka 'Optional<UnsafePointer<Int32>>')}}
}

// Case 7: two parameters and a return sharing a count, mixed.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len"), .countedByOrNull(pointer: .return, count: "len"))
public func twoParamsAndReturn(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) -> UnsafePointer<CInt>? {
// expected-error@+1{{missing return in global function expected to return 'UnsafePointer<CInt>?' (aka 'Optional<UnsafePointer<Int32>>')}}
}

// Case 8: nonescaping (Span) variants — verifies that the unsafe-prefix
// handling for ?.count differs for Span vs UBP.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)))
public func spansAllOptional(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {
}

// Case 9: MutableSpan parameters, mixed nullability. Mutable pointers with
// `.nonescaping` import as `inout MutableSpan<...>` (and `inout MutableSpan<...>?`
// for the OrNull-Nullable variants).
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)))
public func mutSpansMixed(_ p1: UnsafeMutablePointer<CInt>?, _ p2: UnsafeMutablePointer<CInt>?, _ len: CInt) {
}

// Case 10: all-Optional MutableSpan parameters — `??`-chain extraction
// across MutableSpan? values, nil-aware checks (no `unsafe` prefix needed
// because MutableSpan?.count is safe).
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)))
public func mutSpansAllOrNull(_ p1: UnsafeMutablePointer<CInt>?, _ p2: UnsafeMutablePointer<CInt>?, _ len: CInt) {
}

// Case 11: MutableSpan return value sharing a count with MutableSpan
// parameters of mixed nullability. The non-Optional `.countedBy` sharer is
// the extractor; the Optional sharer's check is nil-aware. Return is a
// non-Optional MutableSpan (since `.countedBy` hides nullability).
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)), .countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func mutSpansMixedReturn(_ p1: UnsafeMutablePointer<CInt>?, _ p2: UnsafeMutablePointer<CInt>?, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>')}}
}

// Case 12: MutableSpan? return value — `.countedByOrNull` on the return
// keeps the wrapper's return type Optional, exercising the early-return-nil
// path alongside the shared-count infrastructure.
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)), .countedByOrNull(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func mutSpansMixedOrNullReturn(_ p1: UnsafeMutablePointer<CInt>?, _ p2: UnsafeMutablePointer<CInt>?, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>')}}
}

// Case 13: indirect sharing of `len` between a compound count expression
// (`len + offset`) and a simple count (`len`). The simple sharer extracts
// `len` from p2's count and hides the `len` parameter; the compound sharer
// references the bound `len` in its nil-aware bounds check. Both buffers
// are OrNull-Nullable, so both checks are nil-aware.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len + offset"), .countedByOrNull(pointer: .param(2), count: "len"))
public func compoundAndSimple(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt, _ offset: CInt) {
}

// Case 14: same indirect sharing, but with `.nonescaping` on both so they
// render as Span? — exercises the nil-aware compound check on Span?
// (where `?.count` is safe and no `unsafe` prefix is needed).
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len + offset"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)))
public func compoundAndSimpleSpans(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt, _ offset: CInt) {
}

// Case 15: same again with MutableSpan? — verifies the compound nil-aware
// check works against an extracted simple-count basis when both buffers are
// inout MutableSpan?.
@_SwiftifyImport(.countedByOrNull(pointer: .param(1), count: "len + offset"), .nonescaping(pointer: .param(1)), .countedByOrNull(pointer: .param(2), count: "len"), .nonescaping(pointer: .param(2)))
public func compoundAndSimpleMutSpans(_ p1: UnsafeMutablePointer<CInt>?, _ p2: UnsafeMutablePointer<CInt>?, _ len: CInt, _ offset: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test16nonOptionalFirst15_SwiftifyImportfMp_.swift
------------------------------
// Mix of `.countedBy` (hides nullability — wrapper is non-Optional) and
// `.countedByOrNull` (preserves nullability — wrapper is Optional) sharing
// a count. The non-Optional sharer should be the count extractor regardless
// of declaration order, and the Optional sharer's check should skip when
// it is nil.

// Case 1: non-Optional sharer is first in declaration order.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonOptionalFirst(_ p1: UnsafeBufferPointer<CInt>, _ p2: UnsafeBufferPointer<CInt>?) {
    let len = CInt(exactly: p1.count)!
    if let _p2Count = unsafe p2?.count, _p2Count != len {
      fatalError("bounds check failure in nonOptionalFirst: expected \(len) but got \(_p2Count)")
    }
    return unsafe nonOptionalFirst(p1.baseAddress, p2?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test13optionalFirst15_SwiftifyImportfMp_.swift
------------------------------
// Case 2: Optional sharer is first in declaration order. Generated code
// should bind `len` from `p2` (the non-Optional one) anyway.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func optionalFirst(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != len {
      fatalError("bounds check failure in optionalFirst: expected \(len) but got \(_p1Count)")
    }
    return unsafe optionalFirst(p1?.baseAddress, p2.baseAddress, len)
}
------------------------------
@__swiftmacro_4test11allOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 3: all sharers Optional — `??`-chain extraction, nil-aware checks on
// every non-extractor sharer.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func allOptional(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>?) {
    let len = CInt(exactly: unsafe p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = unsafe p1?.count, _p1Count != len {
      fatalError("bounds check failure in allOptional: expected \(len) but got \(_p1Count)")
    }
    return unsafe allOptional(p1?.baseAddress, p2?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test10threeMixed15_SwiftifyImportfMp_.swift
------------------------------
// Case 4: three sharers with mixed nullability.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func threeMixed(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>, _ p3: UnsafeBufferPointer<CInt>?) {
    let len = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != len {
      fatalError("bounds check failure in threeMixed: expected \(len) but got \(_p1Count)")
    }
    if let _p3Count = unsafe p3?.count, _p3Count != len {
      fatalError("bounds check failure in threeMixed: expected \(len) but got \(_p3Count)")
    }
    return unsafe threeMixed(p1?.baseAddress, p2.baseAddress, p3?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test16threeAllOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 5: three sharers, all Optional — `??`-chain across all three.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func threeAllOptional(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>?, _ p3: UnsafeBufferPointer<CInt>?) {
    let len = CInt(exactly: unsafe p3?.count ?? p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = unsafe p1?.count, _p1Count != len {
      fatalError("bounds check failure in threeAllOptional: expected \(len) but got \(_p1Count)")
    }
    if let _p2Count = unsafe p2?.count, _p2Count != len {
      fatalError("bounds check failure in threeAllOptional: expected \(len) but got \(_p2Count)")
    }
    return unsafe threeAllOptional(p1?.baseAddress, p2?.baseAddress, p3?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test19paramOptionalReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 6: parameter + return value sharing a count, mixed nullability.
// Return uses `len`; parameter extracts it.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func paramOptionalReturn(_ p1: UnsafeBufferPointer<CInt>?) -> UnsafeBufferPointer<CInt> {
    let len = CInt(exactly: unsafe p1?.count ?? 0)!
    return unsafe UnsafeBufferPointer<CInt>(start: unsafe paramOptionalReturn(p1?.baseAddress, len), count: Int(len))
}
------------------------------
@__swiftmacro_4test18twoParamsAndReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 7: two parameters and a return sharing a count, mixed.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func twoParamsAndReturn(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>) -> UnsafeBufferPointer<CInt>? {
    let len = CInt(exactly: p2.count)!
    if let _p1Count = unsafe p1?.count, _p1Count != len {
      fatalError("bounds check failure in twoParamsAndReturn: expected \(len) but got \(_p1Count)")
    }
    let _resultValue = unsafe twoParamsAndReturn(p1?.baseAddress, p2.baseAddress, len)
    if unsafe _resultValue == nil {
      return nil
    }
    return unsafe UnsafeBufferPointer<CInt>(start: _resultValue!, count: Int(len))
}
------------------------------
@__swiftmacro_4test16spansAllOptional15_SwiftifyImportfMp_.swift
------------------------------
// Case 8: nonescaping (Span) variants — verifies that the unsafe-prefix
// handling for ?.count differs for Span vs UBP.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func spansAllOptional(_ p1: Span<CInt>?, _ p2: Span<CInt>?) {
    let len = CInt(exactly: p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = p1?.count, _p1Count != len {
      fatalError("bounds check failure in spansAllOptional: expected \(len) but got \(_p1Count)")
    }
    let _p1Ptr = p1?.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe spansAllOptional(_p1Ptr?.baseAddress, _p2Ptr?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test13mutSpansMixed15_SwiftifyImportfMp_.swift
------------------------------
// Case 9: MutableSpan parameters, mixed nullability. Mutable pointers with
// `.nonescaping` import as `inout MutableSpan<...>` (and `inout MutableSpan<...>?`
// for the OrNull-Nullable variants).
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload
public func mutSpansMixed(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>?) {
    let len = CInt(exactly: p1.count)!
    if let _p2Count = p2?.count, _p2Count != len {
      fatalError("bounds check failure in mutSpansMixed: expected \(len) but got \(_p2Count)")
    }
    let _p1Ptr = p1.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe mutSpansMixed(_p1Ptr.baseAddress, _p2Ptr?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test17mutSpansAllOrNull15_SwiftifyImportfMp_.swift
------------------------------
// Case 10: all-Optional MutableSpan parameters — `??`-chain extraction
// across MutableSpan? values, nil-aware checks (no `unsafe` prefix needed
// because MutableSpan?.count is safe).
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload
public func mutSpansAllOrNull(_ p1: inout MutableSpan<CInt>?, _ p2: inout MutableSpan<CInt>?) {
    let len = CInt(exactly: p2?.count ?? p1?.count ?? 0)!
    if let _p1Count = p1?.count, _p1Count != len {
      fatalError("bounds check failure in mutSpansAllOrNull: expected \(len) but got \(_p1Count)")
    }
    let _p1Ptr = p1?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe mutSpansAllOrNull(_p1Ptr?.baseAddress, _p2Ptr?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test19mutSpansMixedReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 11: MutableSpan return value sharing a count with MutableSpan
// parameters of mixed nullability. The non-Optional `.countedBy` sharer is
// the extractor; the Optional sharer's check is nil-aware. Return is a
// non-Optional MutableSpan (since `.countedBy` hides nullability).
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p1) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload
public func mutSpansMixedReturn(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>?) -> MutableSpan<CInt> {
    let len = CInt(exactly: p1.count)!
    if let _p2Count = p2?.count, _p2Count != len {
      fatalError("bounds check failure in mutSpansMixedReturn: expected \(len) but got \(_p2Count)")
    }
    let _p1Ptr = p1.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    let _resultValue = unsafe mutSpansMixedReturn(_p1Ptr.baseAddress, _p2Ptr?.baseAddress, len)
    if unsafe _resultValue == nil {
      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")
      return MutableSpan<CInt>()
    }
    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test25mutSpansMixedOrNullReturn15_SwiftifyImportfMp_.swift
------------------------------
// Case 12: MutableSpan? return value — `.countedByOrNull` on the return
// keeps the wrapper's return type Optional, exercising the early-return-nil
// path alongside the shared-count infrastructure.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p1) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload
public func mutSpansMixedOrNullReturn(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
    let len = CInt(exactly: p1.count)!
    if let _p2Count = p2?.count, _p2Count != len {
      fatalError("bounds check failure in mutSpansMixedOrNullReturn: expected \(len) but got \(_p2Count)")
    }
    let _p1Ptr = p1.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    let _resultValue = unsafe mutSpansMixedOrNullReturn(_p1Ptr.baseAddress, _p2Ptr?.baseAddress, len)
    if unsafe _resultValue == nil {
      return nil
    }
    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test17compoundAndSimple15_SwiftifyImportfMp_.swift
------------------------------
// Case 13: indirect sharing of `len` between a compound count expression
// (`len + offset`) and a simple count (`len`). The simple sharer extracts
// `len` from p2's count and hides the `len` parameter; the compound sharer
// references the bound `len` in its nil-aware bounds check. Both buffers
// are OrNull-Nullable, so both checks are nil-aware.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func compoundAndSimple(_ p1: UnsafeBufferPointer<CInt>?, _ p2: UnsafeBufferPointer<CInt>?, _ offset: CInt) {
    let len = CInt(exactly: unsafe p2?.count ?? 0)!
    if let _p1Count = unsafe p1?.count, _p1Count != len + offset {
      fatalError("bounds check failure in compoundAndSimple: expected \(len + offset) but got \(_p1Count)")
    }
    return unsafe compoundAndSimple(p1?.baseAddress, p2?.baseAddress, len, offset)
}
------------------------------
@__swiftmacro_4test22compoundAndSimpleSpans15_SwiftifyImportfMp_.swift
------------------------------
// Case 14: same indirect sharing, but with `.nonescaping` on both so they
// render as Span? — exercises the nil-aware compound check on Span?
// (where `?.count` is safe and no `unsafe` prefix is needed).
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func compoundAndSimpleSpans(_ p1: Span<CInt>?, _ p2: Span<CInt>?, _ offset: CInt) {
    let len = CInt(exactly: p2?.count ?? 0)!
    if let _p1Count = p1?.count, _p1Count != len + offset {
      fatalError("bounds check failure in compoundAndSimpleSpans: expected \(len + offset) but got \(_p1Count)")
    }
    let _p1Ptr = p1?.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe compoundAndSimpleSpans(_p1Ptr?.baseAddress, _p2Ptr?.baseAddress, len, offset)
}
------------------------------
@__swiftmacro_4test25compoundAndSimpleMutSpans15_SwiftifyImportfMp_.swift
------------------------------
// Case 15: same again with MutableSpan? — verifies the compound nil-aware
// check works against an extracted simple-count basis when both buffers are
// inout MutableSpan?.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload
public func compoundAndSimpleMutSpans(_ p1: inout MutableSpan<CInt>?, _ p2: inout MutableSpan<CInt>?, _ offset: CInt) {
    let len = CInt(exactly: p2?.count ?? 0)!
    if let _p1Count = p1?.count, _p1Count != len + offset {
      fatalError("bounds check failure in compoundAndSimpleMutSpans: expected \(len + offset) but got \(_p1Count)")
    }
    let _p1Ptr = p1?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p1)
    }
    let _p2Ptr = p2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p2)
    }
    return unsafe compoundAndSimpleMutSpans(_p1Ptr?.baseAddress, _p2Ptr?.baseAddress, len, offset)
}
------------------------------
