// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len"))
public func myFunc(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "len * size"))
public func myFunc2(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .param(2), count: "size"), .countedBy(pointer: .param(3), count: "len * size"))
public func myFunc3(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"), .countedBy(pointer: .param(2), count: "size"), .countedBy(pointer: .param(1), count: "len * size"))
public func myFunc4(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len * size"), .countedBy(pointer: .param(3), count: "len"), .countedBy(pointer: .param(2), count: "size"))
public func myFunc5(_ ptr: UnsafePointer<CInt>, _ ptr2: UnsafePointer<CInt>, _ ptr3: UnsafePointer<CInt>, _ len: CInt, _ size: CInt) {
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    if ptr2.count != len {
      fatalError("bounds check failure in myFunc: expected \(len) but got \(ptr2.count)")
    }
    return unsafe myFunc(ptr.baseAddress!, ptr2.baseAddress!, len)
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc2(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ size: CInt) {
    let len = CInt(exactly: ptr.count)!
    let _ptr2Count = ptr2.count
    if _ptr2Count != len * size {
      fatalError("bounds check failure in myFunc2: expected \(len * size) but got \(_ptr2Count)")
    }
    return unsafe myFunc2(ptr.baseAddress!, ptr2.baseAddress!, len, size)
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc3(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
    let len = CInt(exactly: ptr.count)!
    let size = CInt(exactly: ptr2.count)!
    let _ptr3Count = ptr3.count
    if _ptr3Count != len * size {
      fatalError("bounds check failure in myFunc3: expected \(len * size) but got \(_ptr3Count)")
    }
    return unsafe myFunc3(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc4(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
    let size = CInt(exactly: ptr2.count)!
    let len = CInt(exactly: ptr3.count)!
    let _ptrCount = ptr.count
    if _ptrCount != len * size {
      fatalError("bounds check failure in myFunc4: expected \(len * size) but got \(_ptrCount)")
    }
    return unsafe myFunc4(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
}
------------------------------
@__swiftmacro_4test7myFunc515_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc5(_ ptr: UnsafeBufferPointer<CInt>, _ ptr2: UnsafeBufferPointer<CInt>, _ ptr3: UnsafeBufferPointer<CInt>) {
    let size = CInt(exactly: ptr2.count)!
    let len = CInt(exactly: ptr3.count)!
    let _ptrCount = ptr.count
    if _ptrCount != len * size {
      fatalError("bounds check failure in myFunc5: expected \(len * size) but got \(_ptrCount)")
    }
    return unsafe myFunc5(ptr.baseAddress!, ptr2.baseAddress!, ptr3.baseAddress!, len, size)
}
------------------------------
