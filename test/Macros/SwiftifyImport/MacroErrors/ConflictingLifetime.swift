// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// Hand-written @_lifetime attributes that conflict with the ones the macro
// generates are never replaced: both are emitted, and the compiler rejects them.

// RUN: %target-swift-frontend %s -typecheck -plugin-path %swift-plugin-dir -enable-experimental-feature Lifetimes -verify -Rmacro-expansions -suppress-notes

public struct NE: ~Escapable {
  @_lifetime(immortal)
  public init() {}
}
public struct Esc { var x: CInt = 0 }

// The generated dependence can't be combined with 'immortal'.
// expected-expansion@+19:90{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(immortal, copy p) @_lifetime(p: copy p) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func immortalConflict(_ p: inout MutableSpan<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@6{{macro content: |        unsafe $0|}}
//   expected-remark@7{{macro content: |    }|}}
//   expected-remark@8{{macro content: |    defer {|}}
//   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@10{{macro content: |    }|}}
//   expected-remark@11{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe immortalConflict(_pPtr.baseAddress!, len), copying: ())|}}
//   expected-remark@12{{macro content: |}|}}
//   expected-error@2:50{{cannot specify any other dependence source along with immortal}}
// }}
@_lifetime(immortal)
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"),
                 .nonescaping(pointer: .param(1)),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func immortalConflict(_ p: UnsafeMutablePointer<CInt>, _ len: CInt) -> NE { NE() }

// A bare source is a different spelling of the generated 'borrow p'.
// expected-expansion@+12:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(p, borrow p) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func bareSource(_ p: Esc, _ q: UnsafeMutableBufferPointer<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let len = CInt(exactly: q.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe bareSource(p, q.baseAddress!, len), copying: ())|}}
//   expected-remark@6{{macro content: |}|}}
//   expected-error@2:45{{duplicate lifetime dependence specifier}}
// }}
@_lifetime(p)
@_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow))
public func bareSource(_ p: Esc, _ q: UnsafeMutablePointer<CInt>, _ len: CInt) -> NE { NE() }

// The pointer becomes a Span, so the generated dependence is 'copy ptr'.
// expected-expansion@+18:82{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(borrow ptr, copy ptr) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func pointerToSpan(_ ptr: Span<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let len = CInt(exactly: ptr.count)!|}}
//   expected-remark@5{{macro content: |    let _ptrPtr = ptr.withUnsafeBufferPointer {|}}
//   expected-remark@6{{macro content: |        unsafe $0|}}
//   expected-remark@7{{macro content: |    }|}}
//   expected-remark@8{{macro content: |    defer {|}}
//   expected-remark@9{{macro content: |        _fixLifetime(ptr)|}}
//   expected-remark@10{{macro content: |    }|}}
//   expected-remark@11{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe pointerToSpan(_ptrPtr.baseAddress!, len), copying: ())|}}
//   expected-remark@12{{macro content: |}|}}
//   expected-error@2:52{{duplicate lifetime dependence specifier}}
// }}
@_lifetime(borrow ptr)
@_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .copy))
public func pointerToSpan(_ ptr: UnsafePointer<CInt>, _ len: CInt) -> NE { NE() }

// After renaming, the hand-written 'borrow ne' conflicts with the generated 'copy ne'.
// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(borrow _renamedConflict_param1, copy _renamedConflict_param1) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func renamedConflict(_ _renamedConflict_param0: CInt, _ _renamedConflict_param1: borrowing NE, _ _renamedConflict_param2: UnsafeMutableBufferPointer<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let _renamedConflict_param3 = CInt(exactly: _renamedConflict_param2.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe renamedConflict(_renamedConflict_param0, _renamedConflict_param1, _renamedConflict_param2.baseAddress!, _renamedConflict_param3), copying: ())|}}
//   expected-remark@6{{macro content: |}|}}
//   expected-error@2:72{{duplicate lifetime dependence specifier}}
// }}
@_lifetime(borrow ne)
@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
public func renamedConflict(_: CInt, _ ne: borrowing NE, _ p: UnsafeMutablePointer<CInt>, _ len: CInt) -> NE {
  NE()
}
