// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -verify %t/test.swift
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -strict-memory-safety -dump-macro-expansions %t/test.swift 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func plain(_ ptr: UnsafePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func opt(_ ptr: UnsafePointer<CInt>?) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func mut(_ ptr: UnsafeMutablePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func mutOpt(_ ptr: UnsafeMutablePointer<CInt>?) {}


@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
public func noescape(_ ptr: UnsafePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
public func noescapeOpt(_ ptr: UnsafePointer<CInt>?) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
public func noescapeMut(_ ptr: UnsafeMutablePointer<CInt>) {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"), .nonescaping(pointer: .param(1)))
public func noescapeMutOpt(_ ptr: UnsafeMutablePointer<CInt>?) {}


@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
public func plainReturn() -> UnsafePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
public func optReturn() -> UnsafePointer<CInt>? {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
public func mutReturn() -> UnsafeMutablePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .return, count: "37"))
public func mutOptReturn() -> UnsafeMutablePointer<CInt>? {}


@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func escape(_ ptr: UnsafePointer<CInt>) -> UnsafePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func escapeOpt(_ ptr: UnsafePointer<CInt>?) -> UnsafePointer<CInt>? {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func escapeMut(_ ptr: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "37"))
public func escapeMutOpt(_ ptr: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {}

//--- expansions.expected
@__swiftmacro_4test5plain15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func plain(_ ptr: UnsafeBufferPointer<CInt>) {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in plain: expected \(37) but got \(_ptrCount)")
    }
    return unsafe plain(ptr.baseAddress!)
}
------------------------------
@__swiftmacro_4test3opt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func opt(_ ptr: UnsafeBufferPointer<CInt>?) {
    let _ptrCount = unsafe ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in opt: expected \(37) but got \(_ptrCount)")
    }
    return unsafe opt(ptr?.baseAddress)
}
------------------------------
@__swiftmacro_4test3mut15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mut(_ ptr: UnsafeMutableBufferPointer<CInt>) {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in mut: expected \(37) but got \(_ptrCount)")
    }
    return unsafe mut(ptr.baseAddress!)
}
------------------------------
@__swiftmacro_4test6mutOpt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mutOpt(_ ptr: UnsafeMutableBufferPointer<CInt>?) {
    let _ptrCount = unsafe ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in mutOpt: expected \(37) but got \(_ptrCount)")
    }
    return unsafe mutOpt(ptr?.baseAddress)
}
------------------------------
@__swiftmacro_4test8noescape15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func noescape(_ ptr: Span<CInt>) {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in noescape: expected \(37) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe noescape(_ptrPtr.baseAddress!)
}
------------------------------
@__swiftmacro_4test11noescapeOpt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func noescapeOpt(_ ptr: Span<CInt>?) {
    let _ptrCount = ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in noescapeOpt: expected \(37) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr?.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe noescapeOpt(_ptrPtr?.baseAddress)
}
------------------------------
@__swiftmacro_4test11noescapeMut15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func noescapeMut(_ ptr: inout MutableSpan<CInt>) {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in noescapeMut: expected \(37) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe noescapeMut(_ptrPtr.baseAddress!)
}
------------------------------
@__swiftmacro_4test14noescapeMutOpt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func noescapeMutOpt(_ ptr: inout MutableSpan<CInt>?) {
    let _ptrCount = ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in noescapeMutOpt: expected \(37) but got \(_ptrCount)")
    }
    let _ptrPtr = unsafe ptr?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe noescapeMutOpt(_ptrPtr?.baseAddress)
}
------------------------------
@__swiftmacro_4test11plainReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func plainReturn() -> UnsafeBufferPointer<CInt> {
    return unsafe UnsafeBufferPointer<CInt> (start: unsafe plainReturn(), count: Int(37))
}
------------------------------
@__swiftmacro_4test9optReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func optReturn() -> UnsafeBufferPointer<CInt>? {
    return unsafe { () in
      let _resultValue = unsafe optReturn()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(UnsafeBufferPointer<CInt>(start: _resultValue!, count: Int(37)), copying: ())
      }
    }()
}
------------------------------
@__swiftmacro_4test9mutReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mutReturn() -> UnsafeMutableBufferPointer<CInt> {
    return unsafe UnsafeMutableBufferPointer<CInt> (start: unsafe mutReturn(), count: Int(37))
}
------------------------------
@__swiftmacro_4test12mutOptReturn15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func mutOptReturn() -> UnsafeMutableBufferPointer<CInt>? {
    return unsafe { () in
      let _resultValue = unsafe mutOptReturn()
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(UnsafeMutableBufferPointer<CInt>(start: _resultValue!, count: Int(37)), copying: ())
      }
    }()
}
------------------------------
@__swiftmacro_4test6escape15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func escape(_ ptr: UnsafeBufferPointer<CInt>) -> UnsafePointer<CInt> {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in escape: expected \(37) but got \(_ptrCount)")
    }
    return unsafe escape(ptr.baseAddress!)
}
------------------------------
@__swiftmacro_4test9escapeOpt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func escapeOpt(_ ptr: UnsafeBufferPointer<CInt>?) -> UnsafePointer<CInt>? {
    let _ptrCount = unsafe ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in escapeOpt: expected \(37) but got \(_ptrCount)")
    }
    return unsafe escapeOpt(ptr?.baseAddress)
}
------------------------------
@__swiftmacro_4test9escapeMut15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func escapeMut(_ ptr: UnsafeMutableBufferPointer<CInt>) -> UnsafeMutablePointer<CInt> {
    let _ptrCount = ptr.count
    if _ptrCount != 37 {
      fatalError("bounds check failure in escapeMut: expected \(37) but got \(_ptrCount)")
    }
    return unsafe escapeMut(ptr.baseAddress!)
}
------------------------------
@__swiftmacro_4test12escapeMutOpt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func escapeMutOpt(_ ptr: UnsafeMutableBufferPointer<CInt>?) -> UnsafeMutablePointer<CInt>? {
    let _ptrCount = unsafe ptr?.count ?? 0
    if _ptrCount != 37 {
      fatalError("bounds check failure in escapeMutOpt: expected \(37) but got \(_ptrCount)")
    }
    return unsafe escapeMutOpt(ptr?.baseAddress)
}
------------------------------
