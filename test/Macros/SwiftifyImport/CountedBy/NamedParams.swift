// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func ptrNamed(ptr: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func ptrNamedOther(buf ptr: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func lenNamed(_ ptr: UnsafePointer<CInt>, len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func lenNamedOther(_ ptr: UnsafePointer<CInt>, count len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func allNamed(ptr: UnsafePointer<CInt>, len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func allNamedOther(buf ptr: UnsafePointer<CInt>, count len: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test8ptrNamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func ptrNamed(ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe ptrNamed(ptr: ptr.baseAddress!, len)
}
------------------------------
@__swiftmacro_4test13ptrNamedOther15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func ptrNamedOther(buf ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe ptrNamedOther(buf: ptr.baseAddress!, len)
}
------------------------------
@__swiftmacro_4test8lenNamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func lenNamed(_ ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe lenNamed(ptr.baseAddress!, len: len)
}
------------------------------
@__swiftmacro_4test13lenNamedOther15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func lenNamedOther(_ ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe lenNamedOther(ptr.baseAddress!, count: len)
}
------------------------------
@__swiftmacro_4test8allNamed15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func allNamed(ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe allNamed(ptr: ptr.baseAddress!, len: len)
}
------------------------------
@__swiftmacro_4test13allNamedOther15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func allNamedOther(buf ptr: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    return unsafe allNamedOther(buf: ptr.baseAddress!, count: len)
}
------------------------------
