// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:SizedByLifetimeboundClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift 2> %t/expansions.out
// RUN: diff %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func simple(_ len: Int32, _ p: RawSpan) -> RawSpan {
    let len2 = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe simple(len, len2, _pPtr.baseAddress!)
            }, byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func shared(_ p: RawSpan) -> RawSpan {
    let len = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe shared(len, _pPtr.baseAddress!)
            }, byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: RawSpan) -> RawSpan {
    let len2 = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe complexExpr(len, offset, len2, _pPtr.baseAddress!)
            }, byteCount: Int(len - offset)), copying: ())
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nullUnspecified(_ len: Int32, _ p: RawSpan) -> RawSpan {
    let len2 = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe nullUnspecified(len, len2, _pPtr.baseAddress!)
            }, byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nonnull(_ len: Int32, _ p: RawSpan) -> RawSpan {
    let len2 = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe nonnull(len, len2, _pPtr.baseAddress!)
            }, byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nullable(_ len: Int32, _ p: RawSpan?) -> RawSpan? {
    let len2 = Int32(exactly: p?.byteCount ?? 0)!
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = { () in
              return if p == nil {
                  unsafe nullable(len, len2, nil)
                } else {
                  unsafe p!.withUnsafeBytes { _pPtr in
                    return unsafe nullable(len, len2, _pPtr.baseAddress)
                  }
                }
          }()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())
      }
        }(), copying: ())
}
------------------------------
@__swiftmacro_So6opaque15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func opaque(_ len: Int32, _ p: RawSpan) -> RawSpan {
    let len2 = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe opaque(len, len2, OpaquePointer(_pPtr.baseAddress!))
            }, byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So16nonsizedLifetime15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow p) @_disfavoredOverload public func nonsizedLifetime(_ len: Int32, _ p: UnsafeRawPointer!) -> RawSpan {
    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe nonsizedLifetime(len, p), byteCount: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So9bytesized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func bytesized(_ p: RawSpan) -> MutableRawSpan {
    let size = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe p.withUnsafeBytes { _pPtr in
      return unsafe bytesized(size, _pPtr.baseAddress!.assumingMemoryBound(to: UInt8.self))
            }, byteCount: Int(size)), copying: ())
}
------------------------------
@__swiftmacro_So9charsized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func charsized(_ p: inout MutableRawSpan) -> MutableRawSpan {
    let size = Int32(exactly: p.byteCount)!
    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: unsafe p.withUnsafeMutableBytes { _pPtr in
      return unsafe charsized(_pPtr.baseAddress!.assumingMemoryBound(to: CChar.self), size)
            }, byteCount: Int(size)), copying: ())
}
------------------------------
