// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_Lifetimes

// Hand-written @_lifetime attributes that are compatible with the ones the
// macro generates are kept, and an identical one is emitted only once.

// RUN: %target-swift-frontend %s -emit-module -o /dev/null -plugin-path %swift-plugin-dir -strict-memory-safety -enable-experimental-feature Lifetimes -verify -Rmacro-expansions -suppress-notes

public struct NE: ~Escapable {
  @_lifetime(immortal)
  public init() {}
}

public struct Esc { var x: CInt = 0 }

// 'self' is a valid dependent that is not in the parameter list.
public struct SelfDep: ~Escapable {
  // expected-expansion@+16:79{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |  @_alwaysEmitIntoClient @_lifetime(self: copy self) @_lifetime(p: copy p) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |  public mutating func myFunc(_ p: inout MutableSpan<CInt>) {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
  //   expected-remark@6{{macro content: |        unsafe $0|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |    defer {|}}
  //   expected-remark@9{{macro content: |        _fixLifetime(p)|}}
  //   expected-remark@10{{macro content: |    }|}}
  //   expected-remark@11{{macro content: |    return unsafe myFunc(_pPtr.baseAddress!, len)|}}
  //   expected-remark@12{{macro content: |}|}}
  // }}
  @_lifetime(self: copy self)
  @_SwiftifyImport(.countedBy(pointer: .param(1), count: "len"), .nonescaping(pointer: .param(1)))
  public mutating func myFunc(_ p: UnsafeMutablePointer<CInt>, _ len: CInt) {}
}

// An anonymous parameter renames every parameter, including 'ne'.
// expected-expansion@+12:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(copy _renamed_param1) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func renamed(_ _renamed_param0: CInt, _ _renamed_param1: NE, _ _renamed_param2: UnsafeMutableBufferPointer<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let _renamed_param3 = CInt(exactly: _renamed_param2.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe renamed(_renamed_param0, _renamed_param1, _renamed_param2.baseAddress!, _renamed_param3)|}}
//   expected-remark@6{{macro content: |}|}}
// }}
@_lifetime(copy ne)
@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"))
public func renamed(_: CInt, _ ne: NE, _ p: UnsafeMutablePointer<CInt>, _ len: CInt) -> NE {
  return ne
}

// After renaming, the hand-written dependence is identical to the generated one.
// expected-expansion@+13:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(copy _renamedIdentical_param1) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func renamedIdentical(_ _renamedIdentical_param0: CInt, _ _renamedIdentical_param1: NE, _ _renamedIdentical_param2: UnsafeMutableBufferPointer<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let _renamedIdentical_param3 = CInt(exactly: _renamedIdentical_param2.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe renamedIdentical(_renamedIdentical_param0, _renamedIdentical_param1, _renamedIdentical_param2.baseAddress!, _renamedIdentical_param3), copying: ())|}}
//   expected-remark@6{{macro content: |}|}}
// }}
@_lifetime(copy ne)
@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .copy))
public func renamedIdentical(_: CInt, _ ne: NE, _ p: UnsafeMutablePointer<CInt>, _ len: CInt) -> NE {
  return ne
}

// Two distinct '&' dependences must both survive merging.
// expected-expansion@+15:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(&a, &b) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func twoInout(_ a: inout Esc, _ b: inout Esc, _ p: UnsafeMutableBufferPointer<CInt>) -> NE {|}}
//   expected-remark@4{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe twoInout(&a, &b, p.baseAddress!, len), copying: ())|}}
//   expected-remark@6{{macro content: |}|}}
// }}
@_lifetime(&a, &b)
@_SwiftifyImport(.countedBy(pointer: .param(3), count: "len"),
                 .lifetimeDependence(dependsOn: .param(1), pointer: .return, type: .borrow),
                 .lifetimeDependence(dependsOn: .param(2), pointer: .return, type: .borrow))
public func twoInout(_ a: inout Esc, _ b: inout Esc, _ p: UnsafeMutablePointer<CInt>,
                     _ len: CInt) -> NE {
  return NE()
}
