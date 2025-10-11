// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -dump-macro-expansions 2> %t/tmp.txt
// RUN: %FileCheck --dry-run --ignore-runtime-warnings > %t/expansions.txt < %t/tmp.txt
// RUN: diff --strip-trailing-cr %t/expansions.txt %t/expansions.txt.expected

//--- test.swift
@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
public func nonnullUnsafeRawBufferPointer(_ size: CInt) -> OpaquePointer {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
public func nullableUnsafeRawBufferPointer(_ size: CInt) -> OpaquePointer? {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"))
public func impNullableUnsafeRawBufferPointer(_ size: CInt) -> OpaquePointer! {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"), .sizedBy(pointer: .param(1), size: "size"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func nonnullSpan(p: OpaquePointer, size: CInt) -> OpaquePointer {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"), .sizedBy(pointer: .param(1), size: "size"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func nullableSpan(p: OpaquePointer?, _ size: CInt) -> OpaquePointer? {}

@_SwiftifyImport(.sizedBy(pointer: .return, size: "size"), .sizedBy(pointer: .param(1), size: "size"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func impNullableSpan(p: OpaquePointer!, _ size: CInt) -> OpaquePointer! {}

//--- expansions.txt.expected
@__swiftmacro_4test29nonnullUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nonnullUnsafeRawBufferPointer(_ size: CInt) -> UnsafeRawBufferPointer {
    return unsafe UnsafeRawBufferPointer(start: unsafe UnsafeRawPointer(unsafe nonnullUnsafeRawBufferPointer(size)), count: Int(size))
}
------------------------------
@__swiftmacro_4test30nullableUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func nullableUnsafeRawBufferPointer(_ size: CInt) -> UnsafeRawBufferPointer? {
    return unsafe { () in
      let _resultValue = unsafe nullableUnsafeRawBufferPointer(size)
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(UnsafeRawBufferPointer(start: unsafe UnsafeRawPointer(_resultValue!), count: Int(size)), copying: ())
      }
    }()
}
------------------------------
@__swiftmacro_4test33impNullableUnsafeRawBufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func impNullableUnsafeRawBufferPointer(_ size: CInt) -> UnsafeRawBufferPointer {
    return unsafe UnsafeRawBufferPointer(start: unsafe UnsafeRawPointer(unsafe impNullableUnsafeRawBufferPointer(size)), count: Int(size))
}
------------------------------
@__swiftmacro_4test11nonnullSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
public func nonnullSpan(p: RawSpan) -> RawSpan {
    let size = CInt(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe UnsafeRawPointer(unsafe p.withUnsafeBytes { _pPtr in
      return unsafe nonnullSpan(p: OpaquePointer(_pPtr.baseAddress!), size: size)
                }), byteCount: Int(size)), copying: ())
}
------------------------------
@__swiftmacro_4test12nullableSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
public func nullableSpan(p: RawSpan?) -> RawSpan? {
    let size = CInt(exactly: p?.byteCount ?? 0)!
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = { () in
              return if p == nil {
                  unsafe nullableSpan(p: nil, size)
                } else {
                  unsafe p!.withUnsafeBytes { _pPtr in
                    return unsafe nullableSpan(p: OpaquePointer(_pPtr.baseAddress), size)
                  }
                }
          }()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe UnsafeRawPointer(_resultValue!), byteCount: Int(size)), copying: ())
      }
        }(), copying: ())
}
------------------------------
@__swiftmacro_4test15impNullableSpan15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy p) @_disfavoredOverload
public func impNullableSpan(p: RawSpan) -> RawSpan {
    let size = CInt(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe UnsafeRawPointer(unsafe p.withUnsafeBytes { _pPtr in
      return unsafe impNullableSpan(p: OpaquePointer(_pPtr.baseAddress!), size)
                }), byteCount: Int(size)), copying: ())
}
------------------------------

