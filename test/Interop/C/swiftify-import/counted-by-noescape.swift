// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:CountedByNoEscapeClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes %t/test.swift 2> %t/expansions.out
// RUN: diff --strip-trailing-cr %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe simple(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So9swiftAttr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func swiftAttr(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe swiftAttr(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public func shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p1.count)!
    if p2.count != len {
      fatalError("bounds check failure in shared: expected \(len) but got \(p2.count)")
    }
    return unsafe p2.withUnsafeMutableBufferPointer { _p2Ptr in
      return unsafe p1.withUnsafeMutableBufferPointer { _p1Ptr in
      return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)
      }
    }
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {
    let _pCount = p.count
    if _pCount != len - offset {
      fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
    }
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe complexExpr(len, offset, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe nullUnspecified(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe nonnull(len, _pPtr.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ p: inout MutableSpan<Int32>?) {
    let len = Int32(exactly: p?.count ?? 0)!
    return { () in
        return if p == nil {
            unsafe nullable(len, nil)
          } else {
            unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
              return unsafe nullable(len, _pPtr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So13returnPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))
}
------------------------------
@__swiftmacro_So19returnLifetimeBound15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func returnLifetimeBound(_ len1: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
    let len2 = Int32(exactly: p.count)!
    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe returnLifetimeBound(len1, len2, _pPtr.baseAddress!)
            }, count: Int(len1)), copying: ())
}
------------------------------
@__swiftmacro_So9anonymous15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_anonymous_param1: copy _anonymous_param1) @_disfavoredOverload public func anonymous(_ _anonymous_param1: inout MutableSpan<Int32>?) {
    let _anonymous_param0 = Int32(exactly: _anonymous_param1?.count ?? 0)!
    return { () in
        return if _anonymous_param1 == nil {
            unsafe anonymous(_anonymous_param0, nil)
          } else {
            unsafe _anonymous_param1!.withUnsafeMutableBufferPointer { __anonymous_param1Ptr in
              return unsafe anonymous(_anonymous_param0, __anonymous_param1Ptr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So7keyword15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func keyword(_ `func`: inout MutableSpan<Int32>?, _ `extension`: Int32, _ `init`: Int32, _ open: Int32, _ `var`: Int32, _ `is`: Int32, _ `as`: Int32, _ `in`: Int32, _ `guard`: Int32, _ `where`: Int32) {
    let len = Int32(exactly: `func`?.count ?? 0)!
    return { () in
        return if `func` == nil {
            unsafe keyword(len, nil, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
          } else {
            unsafe `func`!.withUnsafeMutableBufferPointer { _funcPtr in
              return unsafe keyword(len, _funcPtr.baseAddress, `extension`, `init`, open, `var`, `is`, `as`, `in`, `guard`, `where`)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So11pointerName15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_pointerName_param1: copy _pointerName_param1) @_disfavoredOverload public func pointerName(_ _pointerName_param1: inout MutableSpan<Int32>?) {
    let _pointerName_param0 = Int32(exactly: _pointerName_param1?.count ?? 0)!
    return { () in
        return if _pointerName_param1 == nil {
            unsafe pointerName(_pointerName_param0, nil)
          } else {
            unsafe _pointerName_param1!.withUnsafeMutableBufferPointer { __pointerName_param1Ptr in
              return unsafe pointerName(_pointerName_param0, __pointerName_param1Ptr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So7lenName15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_lenName_param2: copy _lenName_param2) @_disfavoredOverload public func lenName(_ _lenName_param0: Int32, _ _lenName_param1: Int32, _ _lenName_param2: inout MutableSpan<Int32>?) {
    let __lenName_param2Count = _lenName_param2?.count ?? 0
    if __lenName_param2Count != _lenName_param0 * _lenName_param1 {
      fatalError("bounds check failure in lenName: expected \(_lenName_param0 * _lenName_param1) but got \(__lenName_param2Count)")
    }
    return { () in
        return if _lenName_param2 == nil {
            unsafe lenName(_lenName_param0, _lenName_param1, nil)
          } else {
            unsafe _lenName_param2!.withUnsafeMutableBufferPointer { __lenName_param2Ptr in
              return unsafe lenName(_lenName_param0, _lenName_param1, __lenName_param2Ptr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So4func15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_func_param1: copy _func_param1) @_disfavoredOverload public func `func`(_ _func_param1: inout MutableSpan<Int32>?) {
    let _func_param0 = Int32(exactly: _func_param1?.count ?? 0)!
    return { () in
        return if _func_param1 == nil {
            unsafe `func`(_func_param0, nil)
          } else {
            unsafe _func_param1!.withUnsafeMutableBufferPointer { __func_param1Ptr in
              return unsafe `func`(_func_param0, __func_param1Ptr.baseAddress)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So11funcRenamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func funcRenamed(`func`: inout MutableSpan<Int32>?, `extension`: Int32, `init`: Int32, open: Int32, `var`: Int32, `is`: Int32, `as`: Int32, `in`: Int32, `guard`: Int32, `where`: Int32) -> UnsafeMutableRawPointer! {
    let len = Int32(exactly: `func`?.count ?? 0)!
    return { () in
        return if `func` == nil {
            unsafe funcRenamed(len: len, func: nil, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
          } else {
            unsafe `func`!.withUnsafeMutableBufferPointer { _funcPtr in
              return unsafe funcRenamed(len: len, func: _funcPtr.baseAddress, extension: `extension`, init: `init`, open: open, var: `var`, is: `is`, as: `as`, in: `in`, guard: `guard`, where: `where`)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So15funcRenamedAnon15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_funcRenamedAnon_param1: copy _funcRenamedAnon_param1) @_disfavoredOverload public func funcRenamedAnon(`func` _funcRenamedAnon_param1: inout MutableSpan<Int32>?, `extension` _funcRenamedAnon_param2: Int32, `init` _funcRenamedAnon_param3: Int32, open _funcRenamedAnon_param4: Int32, `var` _funcRenamedAnon_param5: Int32, `is` _funcRenamedAnon_param6: Int32, `as` _funcRenamedAnon_param7: Int32, `in` _funcRenamedAnon_param8: Int32, `guard` _funcRenamedAnon_param9: Int32, `where` _funcRenamedAnon_param10: Int32) -> UnsafeMutableRawPointer! {
    let _funcRenamedAnon_param0 = Int32(exactly: _funcRenamedAnon_param1?.count ?? 0)!
    return { () in
        return if _funcRenamedAnon_param1 == nil {
            unsafe funcRenamedAnon(len: _funcRenamedAnon_param0, func: nil, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)
          } else {
            unsafe _funcRenamedAnon_param1!.withUnsafeMutableBufferPointer { __funcRenamedAnon_param1Ptr in
              return unsafe funcRenamedAnon(len: _funcRenamedAnon_param0, func: __funcRenamedAnon_param1Ptr.baseAddress, extension: _funcRenamedAnon_param2, init: _funcRenamedAnon_param3, open: _funcRenamedAnon_param4, var: _funcRenamedAnon_param5, is: _funcRenamedAnon_param6, as: _funcRenamedAnon_param7, in: _funcRenamedAnon_param8, guard: _funcRenamedAnon_param9, where: _funcRenamedAnon_param10)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So5clash15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func clash(`func`: inout MutableSpan<Int32>?, clash `where`: Int32) {
    let len = Int32(exactly: `func`?.count ?? 0)!
    return { () in
        return if `func` == nil {
            unsafe clash(len: len, func: nil, clash: `where`)
          } else {
            unsafe `func`!.withUnsafeMutableBufferPointer { _funcPtr in
              return unsafe clash(len: len, func: _funcPtr.baseAddress, clash: `where`)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So4open15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(`func`: copy `func`) @_disfavoredOverload public func open(`func`: inout MutableSpan<Int32>?, open `where`: Int32) {
    let len = Int32(exactly: `func`?.count ?? 0)!
    return { () in
        return if `func` == nil {
            unsafe open(len: len, func: nil, open: `where`)
          } else {
            unsafe `func`!.withUnsafeMutableBufferPointer { _funcPtr in
              return unsafe open(len: len, func: _funcPtr.baseAddress, open: `where`)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So6clash215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_clash2_param1: copy _clash2_param1) @_disfavoredOverload public func clash2(`func` _clash2_param1: inout MutableSpan<Int32>?, clash2 _clash2_param2: Int32) {
    let _clash2_param0 = Int32(exactly: _clash2_param1?.count ?? 0)!
    return { () in
        return if _clash2_param1 == nil {
            unsafe clash2(len: _clash2_param0, func: nil, clash2: _clash2_param2)
          } else {
            unsafe _clash2_param1!.withUnsafeMutableBufferPointer { __clash2_param1Ptr in
              return unsafe clash2(len: _clash2_param0, func: __clash2_param1Ptr.baseAddress, clash2: _clash2_param2)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So2in15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_in_param1: copy _in_param1) @_disfavoredOverload public func `in`(`func` _in_param1: inout MutableSpan<Int32>?, `in` _in_param2: Int32) {
    let _in_param0 = Int32(exactly: _in_param1?.count ?? 0)!
    return { () in
        return if _in_param1 == nil {
            unsafe `in`(len: _in_param0, func: nil, in: _in_param2)
          } else {
            unsafe _in_param1!.withUnsafeMutableBufferPointer { __in_param1Ptr in
              return unsafe `in`(len: _in_param0, func: __in_param1Ptr.baseAddress, in: _in_param2)
            }
          }
    }()
}
------------------------------
@__swiftmacro_So11keywordType15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func keywordType(_ p: inout MutableSpan<actor?>, _ p2: actor) -> actor {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe keywordType(len, _pPtr.baseAddress!, p2)
    }
}
------------------------------
