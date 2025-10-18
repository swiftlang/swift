// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:CountedByLifetimeboundClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift 2> %t/expansions.out
// RUN: diff --strip-trailing-cr %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len2 = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe simple(len, len2, _pPtr.baseAddress!)
            }, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func shared(_ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe shared(len, _pPtr.baseAddress!)
            }, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len2 = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe complexExpr(len, offset, len2, _pPtr.baseAddress!)
            }, count: Int(len - offset)), copying: ())
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len2 = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe nullUnspecified(len, len2, _pPtr.baseAddress!)
            }, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len2 = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe nonnull(len, len2, _pPtr.baseAddress!)
            }, count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ len: Int32, _ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {
    let len2 = Int32(exactly: p?.count ?? 0)!
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = { () in
              return if p == nil {
                  unsafe nullable(len, len2, nil)
                } else {
                  unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
                    return unsafe nullable(len, len2, _pPtr.baseAddress)
                  }
                }
          }()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
      }
        }(), copying: ())
}
------------------------------
@__swiftmacro_So18noncountedLifetime15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow p) @_disfavoredOverload public func noncountedLifetime(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> MutableSpan<Int32> {
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe noncountedLifetime(len, p), count: Int(len)), copying: ())
}
------------------------------
@__swiftmacro_So8constant15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func constant(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {
    let _pCount = p?.count ?? 0
    if _pCount != 13 {
      fatalError("bounds check failure in constant: expected \(13) but got \(_pCount)")
    }
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = { () in
              return if p == nil {
                  unsafe constant(nil)
                } else {
                  unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
                    return unsafe constant(_pPtr.baseAddress)
                  }
                }
          }()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: _resultValue!, count: Int(13)), copying: ())
      }
        }(), copying: ())
}
------------------------------
