// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Cells

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Cells -disable-availability-checking -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Cells -disable-availability-checking -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), .reference(pointer: .return))
public func mutableRef(_ a: inout CInt, _ b: inout CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

@_SwiftifyImport(.lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow), .reference(pointer: .return))
public func constRef(_ a: borrowing CInt) -> UnsafePointer<CInt> {
  unsafe UnsafePointer(bitPattern: 8)!
}

@_SwiftifyImport(.reference(pointer: .return), .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .borrow),
                 refAvailability: "macOS 9999, iOS 9999")
public func available(_ a: inout CInt, _ b: inout CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

// Both a Span parameter and a Ref return value: Ref is newer, so its availability wins.
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)),
                 .reference(pointer: .return), .lifetimeDependence(dependsOn: .param(3), pointer: .return, type: .borrow),
                 spanAvailability: "macOS 26.0, iOS 26.0, visionOS 26.0",
                 refAvailability: "anyAppleOS 9999")
public func spanAndRef(_ p: UnsafePointer<CInt>, _ len: CInt, _ a: inout CInt) -> UnsafeMutablePointer<CInt> {
  unsafe UnsafeMutablePointer(bitPattern: 8)!
}

public struct S {
  var x: CInt

  @_SwiftifyImport(.lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow), .reference(pointer: .return))
  public mutating func get() -> UnsafeMutablePointer<CInt> {
    unsafe UnsafeMutablePointer(bitPattern: 8)!
  }

  @_SwiftifyImport(.lifetimeDependence(dependsOn: .self, pointer: .return, type: .borrow), .reference(pointer: .return))
  public borrowing func cget() -> UnsafePointer<CInt> {
    unsafe UnsafePointer(bitPattern: 8)!
  }
}

public func use(_ s: inout S, _ x: inout CInt, _ y: inout CInt) {
  let r: Ref<Cell<CInt>> = mutableRef(&x, &y)
  r.value.value = 1
  let c: Ref<ConstCell<CInt>> = s.cget()
  _ = c.value.value
}

//--- expansions.expected
@__swiftmacro_4test10mutableRef15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_lifetime(&a) @_disfavoredOverload
public func mutableRef(_ a: inout CInt, _ b: inout CInt) -> Swift::Ref<Swift::Cell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe mutableRef(&a, &b)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
@__swiftmacro_4test8constRef15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @_lifetime(borrow a) @_disfavoredOverload
public func constRef(_ a: borrowing @_addressable CInt) -> Swift::Ref<Swift::ConstCell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe constRef(a)).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
@__swiftmacro_4test9available15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @available(macOS 9999, iOS 9999, *) @_lifetime(&b) @_disfavoredOverload
public func available(_ a: inout CInt, _ b: inout CInt) -> Swift::Ref<Swift::Cell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe available(&a, &b)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
@__swiftmacro_4test10spanAndRef15_SwiftifyImportfMp_.swift
------------------------------
// Both a Span parameter and a Ref return value: Ref is newer, so its availability wins.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @inline(always) @available(anyAppleOS 9999, *) @_lifetime(&a) @_disfavoredOverload
public func spanAndRef(_ p: Span<CInt>, _ a: inout CInt) -> Swift::Ref<Swift::Cell<CInt>> {
    let len = CInt(exactly: p.count)!
    let _pPtr = p.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(p)
    }
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe spanAndRef(_pPtr.baseAddress!, len, &a)).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
@__swiftmacro_4test1SV3get15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
  @_alwaysEmitIntoClient @inline(always) @_lifetime(&self) @_disfavoredOverload
  public mutating func get() -> Swift::Ref<Swift::Cell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe get()).assumingMemoryBound(to: Swift::Cell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
@__swiftmacro_4test1SV4cget15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
  @_alwaysEmitIntoClient @inline(always) @_addressableSelf @_lifetime(borrow self) @_disfavoredOverload
  public borrowing func cget() -> Swift::Ref<Swift::ConstCell<CInt>> {
    return unsafe _swiftifyOverrideLifetime(Swift::Ref(unsafeAddress: UnsafeRawPointer(unsafe cget()).assumingMemoryBound(to: Swift::ConstCell<CInt>.self), borrowing: ()), copying: ())
}
------------------------------
