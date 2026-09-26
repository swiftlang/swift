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
// The result depends on a ~Escapable parameter, not on the pointer, so the
// hand-written dependence is identical to the generated one.
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .lifetimeDependence(dependsOn: .param(3), pointer: .return, type: .copy))
@_lifetime(copy ne)
public func myFunc(_ ptr: UnsafePointer<CInt>, _ len: CInt, _ ne: NonescapableEnum) -> NonescapableEnum {
  return .foo
}

@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)), .lifetimeDependence(dependsOn: .param(3), pointer: .return, type: .copy))
@_lifetime(extraNE: copy extraNE) @_lifetime(copy extraNE)
public func myFunc2(_ ptr: UnsafeMutablePointer<CInt>, _ len: CInt, _ extraNE: inout NonescapableEnum) -> NonescapableEnum {
  return .foo
}

//--- expansions.expected
@__swiftmacro_4test6myFunc15_SwiftifyImportfMp_.swift
------------------------------
// The result depends on a ~Escapable parameter, not on the pointer, so the
// hand-written dependence is identical to the generated one.
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy ne) @_disfavoredOverload
public func myFunc(_ ptr: Span<CInt>, _ ne: NonescapableEnum) -> NonescapableEnum {
    let len = CInt(exactly: ptr.count)!
    let _ptrPtr = ptr.withUnsafeBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(unsafe myFunc(_ptrPtr.baseAddress!, len, ne), copying: ())
}
------------------------------
@__swiftmacro_4test7myFunc215_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_lifetime(copy extraNE) @_lifetime(ptr: copy ptr) @_lifetime(extraNE: copy extraNE) @_disfavoredOverload
public func myFunc2(_ ptr: inout MutableSpan<CInt>, _ extraNE: inout NonescapableEnum) -> NonescapableEnum {
    let len = CInt(exactly: ptr.count)!
    let _ptrPtr = ptr.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    defer {
        _fixLifetime(ptr)
    }
    return unsafe _swiftifyOverrideLifetime(unsafe myFunc2(_ptrPtr.baseAddress!, len, &extraNE), copying: ())
}
------------------------------
