// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes -verify
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend %t/test.swift -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Lifetimes -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.swift
public enum NonescapableEnum: ~Escapable {
  case foo
}
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
@_lifetime(borrow ptr)
public func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt) -> NonescapableEnum {
  return .foo
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy), .lifetimeDependence(dependsOn: .param(3), pointer: .return, type: .copy))
@_lifetime(extraNE: copy extraNE) @_lifetime(borrow ptr, copy extraNE)
public func myFunc2(_ ptr: UnsafeMutablePointer<CInt>, _ len: CInt, _ extraNE: inout NonescapableEnum) -> NonescapableEnum {
  return .foo
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr) @_disfavoredOverload
public func myFunc(_ ptr: Span<CInt>) -> NonescapableEnum {
    let len = CInt(exactly: ptr.count)!
    let _ptrPtr = unsafe ptr.withUnsafeBufferPointer {
        unsafe $0
    }
    return unsafe _swiftifyOverrideLifetime(unsafe myFunc(_ptrPtr.baseAddress!, len), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ptr, copy extraNE) @_lifetime(ptr: copy ptr) @_lifetime(extraNE: copy extraNE) @_disfavoredOverload
public func myFunc2(_ ptr: inout MutableSpan<CInt>, _ extraNE: inout NonescapableEnum) -> NonescapableEnum {
    let len = CInt(exactly: ptr.count)!
    let _ptrPtr = unsafe ptr.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe _swiftifyOverrideLifetime(unsafe myFunc2(_ptrPtr.baseAddress!, len, &extraNE), copying: ())
}
------------------------------
