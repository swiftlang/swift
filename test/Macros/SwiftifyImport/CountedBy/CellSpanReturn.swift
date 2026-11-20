// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Cells
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Cells -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t%{fs-sep}test.swift -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Cells -enable-experimental-feature Lifetimes -dump-macro-expansions 2> %t%{fs-sep}expansions.out
// RUN: %diff %t%{fs-sep}expansions.out %t%{fs-sep}expansions.expected

//--- test.swift
// Passing refAvailability opts into Span<[Const]Cell<T>> return values. It is empty without
// availability info.
@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 refAvailability: "")
public func mutableSpan(_ a: borrowing CInt, _ len: CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

// refAvailability replaces spanAvailability.
@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 spanAvailability: "macOS 9999", refAvailability: "macOS 10.14.4, iOS 12.2")
public func constSpan(_ a: borrowing CInt, _ len: CInt) -> UnsafePointer<CInt> {
  unsafe UnsafePointer(bitPattern: 8)!
}

@_SwiftifyImport(.countedByOrNull(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 nullableAsEmptySpan: true, refAvailability: "")
public func nullable(_ a: borrowing CInt, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
  nil
}

@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 nullableAsEmptySpan: true, refAvailability: "")
public func nullUnspecified(_ a: borrowing CInt, _ len: CInt) -> UnsafePointer<CInt>! {
  nil
}

// Without a lifetime dependence the result is not a Span, and refAvailability does not apply.
@_SwiftifyImport(.countedBy(pointer: .return, count: "len"), refAvailability: "macOS 9999")
public func noDependence(_ len: CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

// sizedBy has no element type to wrap in a Cell, so spanAvailability applies.
@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 spanAvailability: "macOS 10.14.4", refAvailability: "macOS 9999")
public func rawSpan(_ a: borrowing CInt, _ size: CInt) -> UnsafeMutableRawPointer {
  unsafe UnsafeMutableRawPointer(bitPattern: 8)!
}

public func use(_ x: CInt) {
  let s: Span<Cell<CInt>> = mutableSpan(x, 1)
  s[0].value = 1
  let c: Span<ConstCell<CInt>> = constSpan(x, 1)
  _ = c[0].value
}

//--- expansions.expected
@__swiftmacro_4test11mutableSpan15_SwiftifyImportfMp_.swift
------------------------------
// Passing refAvailability opts into Span<[Const]Cell<T>> return values. It is empty without
// availability info.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_lifetime(borrow a) @_disfavoredOverload
public func mutableSpan(_ a: borrowing @_addressable CInt, _ len: CInt) -> Span<Swift::Cell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Span<Swift::Cell<CInt>>(_unsafeStart: UnsafeRawPointer(unsafe mutableSpan(a, len)).assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test9constSpan15_SwiftifyImportfMp_.swift
------------------------------
// refAvailability replaces spanAvailability.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @available(macOS 10.14.4, iOS 12.2, *) @_lifetime(borrow a) @_disfavoredOverload
public func constSpan(_ a: borrowing @_addressable CInt, _ len: CInt) -> Span<Swift::ConstCell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Span<Swift::ConstCell<CInt>>(_unsafeStart: UnsafeRawPointer(unsafe constSpan(a, len)).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_lifetime(borrow a) @_disfavoredOverload
public func nullable(_ a: borrowing @_addressable CInt, _ len: CInt) -> Span<Swift::Cell<CInt>>? {
    let _resultValue = unsafe nullable(a, len)
    if unsafe _resultValue == nil {
      return nil
    }
    return unsafe _swiftifyOverrideLifetime(Span<Swift::Cell<CInt>>(_unsafeStart: UnsafeRawPointer(_resultValue!).assumingMemoryBound(to: Swift::Cell<CInt>.self), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_lifetime(borrow a) @_disfavoredOverload
public func nullUnspecified(_ a: borrowing @_addressable CInt, _ len: CInt) -> Span<Swift::ConstCell<CInt>> {
    let _resultValue: UnsafePointer<CInt>? = unsafe nullUnspecified(a, len)
    if unsafe _resultValue == nil {
      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")
      return Span<Swift::ConstCell<CInt>>()
    }
    return unsafe _swiftifyOverrideLifetime(Span<Swift::ConstCell<CInt>>(_unsafeStart: UnsafeRawPointer(_resultValue!).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_4test12noDependence15_SwiftifyImportfMp_.swift
------------------------------
// Without a lifetime dependence the result is not a Span, and refAvailability does not apply.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload
public func noDependence(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
    return unsafe UnsafeMutableBufferPointer<CInt> (start: unsafe noDependence(len), count: Int(len))
}
------------------------------
@__swiftmacro_4test7rawSpan15_SwiftifyImportfMp_.swift
------------------------------
// sizedBy has no element type to wrap in a Cell, so spanAvailability applies.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @available(macOS 10.14.4, *) @_lifetime(borrow a) @_disfavoredOverload
public func rawSpan(_ a: borrowing @_addressable CInt, _ size: CInt) -> MutableRawSpan {
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe rawSpan(a, size), byteCount: Int(size)), copying: ())
}
------------------------------
