// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"))
public func myFunc(_ ptr: UnsafePointer<CInt>?, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
public func myFunc2(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .countedBy(pointer: .param(3), count: "len2"), .nonescaping(pointer: .param(3)))
public func myFunc3(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt, _ ptr2: UnsafeMutablePointer<CInt>?, _ len2: CInt) {
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .countedBy(pointer: .return, count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func myFunc4(_ ptr: UnsafeMutablePointer<CInt>?, _ len: CInt) -> UnsafeMutablePointer<CInt>? {
// expected-error@+1{{missing return in global function expected to return 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>')}}
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
public func myFunc(_ ptr: UnsafeBufferPointer<CInt>?) {
    let len = CInt(exactly: unsafe ptr?.count ?? 0)!
    return unsafe myFunc(ptr?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func myFunc2(_ ptr: inout MutableSpan<CInt>?) {
    let len = CInt(exactly: ptr?.count ?? 0)!
    let _ptrPtr = unsafe ptr?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe myFunc2(_ptrPtr?.baseAddress, len)
}
------------------------------
@__swiftmacro_4test7myFunc315_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(ptr: copy ptr) @_lifetime(ptr2: copy ptr2) @_disfavoredOverload
public func myFunc3(_ ptr: inout MutableSpan<CInt>?, _ ptr2: inout MutableSpan<CInt>?) {
    let len = CInt(exactly: ptr?.count ?? 0)!
    let len2 = CInt(exactly: ptr2?.count ?? 0)!
    let _ptrPtr = unsafe ptr?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    let _ptr2Ptr = unsafe ptr2?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr2)
    }
    return unsafe myFunc3(_ptrPtr?.baseAddress, len, _ptr2Ptr?.baseAddress, len2)
}
------------------------------
@__swiftmacro_4test7myFunc415_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_lifetime(ptr: copy ptr) @_disfavoredOverload
public func myFunc4(_ ptr: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
    let len = CInt(exactly: ptr?.count ?? 0)!
    let _ptrPtr = unsafe ptr?.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime({ () in
      let _resultValue = unsafe myFunc4(_ptrPtr?.baseAddress, len)
      if unsafe _resultValue == nil {
        return nil
      } else {
        return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())
      }
        }(), copying: ())
}
------------------------------
