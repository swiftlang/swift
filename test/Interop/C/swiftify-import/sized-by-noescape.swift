// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:SizedByNoEscapeClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift 2> %t/expansions.out
// RUN: diff %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func simple(_ p: RawSpan) {
    let len = Int32(exactly: p.byteCount)!
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe simple(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So9swiftAttr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func swiftAttr(_ p: RawSpan) {
    let len = Int32(exactly: p.byteCount)!
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe swiftAttr(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func shared(_ p1: RawSpan, _ p2: RawSpan) {
    let len = Int32(exactly: p1.byteCount)!
    if p2.byteCount != len {
      fatalError("bounds check failure in shared: expected \(len) but got \(p2.byteCount)")
    }
    return unsafe p2.withUnsafeBytes { _p2Ptr in
      return unsafe p1.withUnsafeBytes { _p1Ptr in
      return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)
      }
    }
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: RawSpan) {
    let _pCount = p.byteCount
    if _pCount != len - offset {
      fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
    }
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe complexExpr(len, offset, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nullUnspecified(_ p: RawSpan) {
    let len = Int32(exactly: p.byteCount)!
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe nullUnspecified(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nonnull(_ p: RawSpan) {
    let len = Int32(exactly: p.byteCount)!
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe nonnull(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nullable(_ p: RawSpan?) {
    let len = Int32(exactly: p?.byteCount ?? 0)!
    return { () in
        return if p == nil {
            unsafe nullable(len, nil)
          } else {
            unsafe p!.withUnsafeBytes { _pPtr in
              return unsafe nullable(len, _pPtr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So13returnPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeRawBufferPointer {
    return unsafe UnsafeRawBufferPointer(start: unsafe returnPointer(len), count: Int(len))
}
------------------------------
@__swiftmacro_So6opaque15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func opaque(_ p: RawSpan) {
    let len = Int32(exactly: p.byteCount)!
    return unsafe p.withUnsafeBytes { _pPtr in
      return unsafe opaque(len, OpaquePointer(_pPtr.baseAddress!))
    }
}
------------------------------
@__swiftmacro_So9bytesized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func bytesized(_ _bytesized_param1: RawSpan) {
    let _bytesized_param0 = Int32(exactly: _bytesized_param1.byteCount)!
    return unsafe _bytesized_param1.withUnsafeBytes { __bytesized_param1Ptr in
      return unsafe bytesized(_bytesized_param0, __bytesized_param1Ptr.baseAddress!.assumingMemoryBound(to: UInt8.self))
    }
}
------------------------------
@__swiftmacro_So9charsized15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_charsized_param0: copy _charsized_param0) @_disfavoredOverload public func charsized(_ _charsized_param0: inout MutableRawSpan) {
    let _charsized_param1 = Int32(exactly: _charsized_param0.byteCount)!
    return unsafe _charsized_param0.withUnsafeMutableBytes { __charsized_param0Ptr in
      return unsafe charsized(__charsized_param0Ptr.baseAddress!.assumingMemoryBound(to: CChar.self), _charsized_param1)
    }
}
------------------------------
