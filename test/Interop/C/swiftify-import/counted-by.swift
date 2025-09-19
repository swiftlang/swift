// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %generate-callers(module:CountedByClang) > %t/test.swift
// RUN: %verify-safe-wrappers -enable-experimental-feature SafeInteropWrappers %t/test.swift
// RUN: %dump-safe-wrappers -enable-experimental-feature SafeInteropWrappers %t/test.swift 2> %t/expansions.out
// RUN: diff %t/expansions.out %t/expansions.expected

//--- expansions.expected
@__swiftmacro_So6simple15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe simple(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So13simpleFlipped15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func simpleFlipped(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe simpleFlipped(p.baseAddress!, len)
}
------------------------------
@__swiftmacro_So9swiftAttr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe swiftAttr(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So6shared15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p1.count)!
    if unsafe p2.count != len {
      fatalError("bounds check failure in shared: expected \(len) but got \(unsafe p2.count)")
    }
    return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)
}
------------------------------
@__swiftmacro_So11complexExpr15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != len - offset {
      fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
    }
    return unsafe complexExpr(len, offset, p.baseAddress!)
}
------------------------------
@__swiftmacro_So15nullUnspecified15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe nullUnspecified(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So7nonnull15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {
    let len = Int32(exactly: unsafe p.count)!
    return unsafe nonnull(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So8nullable15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {
    let len = Int32(exactly: unsafe p?.count ?? 0)!
    return unsafe nullable(len, p?.baseAddress)
}
------------------------------
@__swiftmacro_So13returnPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))
}
------------------------------
@__swiftmacro_So8offByOne15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func offByOne(_ len: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != len + 1 {
      fatalError("bounds check failure in offByOne: expected \(len + 1) but got \(_pCount)")
    }
    return unsafe offByOne(len, p.baseAddress!)
}
------------------------------
@__swiftmacro_So9offBySome15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func offBySome(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != len + (1 + offset) {
      fatalError("bounds check failure in offBySome: expected \(len + (1 + offset)) but got \(_pCount)")
    }
    return unsafe offBySome(len, offset, p.baseAddress!)
}
------------------------------
@__swiftmacro_So6scalar15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func scalar(_ m: Int32, _ n: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != m * n {
      fatalError("bounds check failure in scalar: expected \(m * n) but got \(_pCount)")
    }
    return unsafe scalar(m, n, p.baseAddress!)
}
------------------------------
@__swiftmacro_So7bitwise15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func bitwise(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != m & n | ~o {
      fatalError("bounds check failure in bitwise: expected \(m & n | ~o) but got \(_pCount)")
    }
    return unsafe bitwise(m, n, o, p.baseAddress!)
}
------------------------------
@__swiftmacro_So8bitshift15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func bitshift(_ m: Int32, _ n: Int32, _ o: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != m << (n >> o) {
      fatalError("bounds check failure in bitshift: expected \(m << (n >> o)) but got \(_pCount)")
    }
    return unsafe bitshift(m, n, o, p.baseAddress!)
}
------------------------------
@__swiftmacro_So8constInt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func constInt(_ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != 420 {
      fatalError("bounds check failure in constInt: expected \(420) but got \(_pCount)")
    }
    return unsafe constInt(p.baseAddress!)
}
------------------------------
@__swiftmacro_So21constFloatCastedToInt15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func constFloatCastedToInt(_ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != 0 {
      fatalError("bounds check failure in constFloatCastedToInt: expected \(0) but got \(_pCount)")
    }
    return unsafe constFloatCastedToInt(p.baseAddress!)
}
------------------------------
@__swiftmacro_So10hexLiteral15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func hexLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != 250 {
      fatalError("bounds check failure in hexLiteral: expected \(250) but got \(_pCount)")
    }
    return unsafe hexLiteral(p.baseAddress!)
}
------------------------------
@__swiftmacro_So13binaryLiteral15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func binaryLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != 2 {
      fatalError("bounds check failure in binaryLiteral: expected \(2) but got \(_pCount)")
    }
    return unsafe binaryLiteral(p.baseAddress!)
}
------------------------------
@__swiftmacro_So12octalLiteral15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func octalLiteral(_ p: UnsafeMutableBufferPointer<Int32>) {
    let _pCount = unsafe p.count
    if _pCount != 511 {
      fatalError("bounds check failure in octalLiteral: expected \(511) but got \(_pCount)")
    }
    return unsafe octalLiteral(p.baseAddress!)
}
------------------------------
