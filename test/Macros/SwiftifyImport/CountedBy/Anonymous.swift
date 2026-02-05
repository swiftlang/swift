// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func myFunc(_: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func myFunc2(_ p: UnsafePointer<CInt>, _ len: CInt, _: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func myFunc3(_: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func myFunc4(_: UnsafeMutablePointer<CInt>, _ len: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ _myFunc_param0: UnsafeBufferPointer<CInt>) {
    let _myFunc_param1 = CInt(exactly: _myFunc_param0.count)!
    return unsafe myFunc(_myFunc_param0.baseAddress!, _myFunc_param1)
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc2(_ _myFunc2_param0: UnsafeBufferPointer<CInt>, _ _myFunc2_param2: CInt) {
    let _myFunc2_param1 = CInt(exactly: _myFunc2_param0.count)!
    return unsafe myFunc2(_myFunc2_param0.baseAddress!, _myFunc2_param1, _myFunc2_param2)
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc3(_ _myFunc3_param0: Span<CInt>) {
    let _myFunc3_param1 = CInt(exactly: _myFunc3_param0.count)!
    return unsafe _myFunc3_param0.withUnsafeBufferPointer { __myFunc3_param0Ptr in
      return unsafe myFunc3(__myFunc3_param0Ptr.baseAddress!, _myFunc3_param1)
    }
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(_myFunc4_param0: copy _myFunc4_param0) @_disfavoredOverload
public func myFunc4(_ _myFunc4_param0: inout MutableSpan<CInt>) {
    let _myFunc4_param1 = CInt(exactly: _myFunc4_param0.count)!
    return unsafe _myFunc4_param0.withUnsafeMutableBufferPointer { __myFunc4_param0Ptr in
      return unsafe myFunc4(__myFunc4_param0Ptr.baseAddress!, _myFunc4_param1)
    }
}
------------------------------
