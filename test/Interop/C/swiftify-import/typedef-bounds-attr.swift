// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking -Xcc -Wno-nullability-completeness \
// RUN:   -verify-additional-prefix attr-

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking -Xcc -Wno-nullability-completeness \
// RUN:   -verify-additional-prefix bounds- -Xcc -fbounds-safety -disable-objc-interop

// Regression test for making sure we handle the hacky synthetic typedefs that
// clang generates when applying bounds attributes to typedefs.

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __single __attribute__((__single__))

typedef struct foo *fooptr_t;
typedef fooptr_t _Nonnull layer2_t;
typedef void *voidptr_t;
typedef char *charptr_t;

// expected-expansion@+7:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr(_ p: UnsafeRawBufferPointer) -> OpaquePointer! {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_fooptr(len, OpaquePointer(p.baseAddress))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
fooptr_t __single test_fooptr(int len, fooptr_t __sized_by(len) p);

// expected-expansion@+7:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr2(_ p: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe UnsafeRawBufferPointer(start: unsafe UnsafeRawPointer(unsafe test_fooptr2(len, OpaquePointer(p.baseAddress))), count: Int(CInt(4)))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
fooptr_t __single __sized_by(4) test_fooptr2(int len, fooptr_t __sized_by(len) p);

// expected-expansion@+7:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr3(_ p: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe UnsafeRawBufferPointer(start: unsafe UnsafeRawPointer(unsafe test_fooptr3(len, OpaquePointer(p.baseAddress))), count: Int(CInt(4)))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
fooptr_t __sized_by(4) test_fooptr3(int len, fooptr_t __sized_by(len) p);

// expected-expansion@+7:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr4(_ p: UnsafeRawBufferPointer) -> OpaquePointer {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_fooptr4(len, OpaquePointer(p.baseAddress))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
fooptr_t __single _Nonnull test_fooptr4(int len, fooptr_t __sized_by(len) p);

// expected-expansion@+9:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr5(_ p: UnsafeRawBufferPointer) -> OpaquePointer {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   Diverges due to https://github.com/swiftlang/llvm-project/issues/13277
//   expected-attr-remark@4{{macro content: |    return unsafe test_fooptr5(len, OpaquePointer(p.baseAddress))|}}
//   expected-bounds-remark@4{{macro content: |    return unsafe test_fooptr5(len, OpaquePointer(p.baseAddress!))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
layer2_t __single test_fooptr5(int len, layer2_t __sized_by(len) p);

// expected-expansion@+7:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_voidptr(_ p: UnsafeMutableRawBufferPointer) -> UnsafeMutableRawPointer! {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_voidptr(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
voidptr_t __single test_voidptr(int len, voidptr_t __sized_by(len) p);

// expected-expansion@+7:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_charptr(_ p: UnsafeMutableRawBufferPointer) -> UnsafeMutablePointer<CChar>! {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_charptr(len, p.baseAddress?.assumingMemoryBound(to: CChar.self))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
charptr_t __single test_charptr(int len, charptr_t __sized_by(len) p);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 02cc53270b917da2e2b202f5f46430c1a1a4a2ba8e283f29dcc7a96f215ea7b7
import Test


func call_test_fooptr(_ len: CInt, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe test_fooptr(len, p)
}

func call_test_fooptr2(_ len: CInt, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe test_fooptr2(len, p)
}

func call_test_fooptr3(_ len: CInt, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe test_fooptr3(len, p)
}

func call_test_fooptr4(_ len: CInt, _ p: OpaquePointer!) -> OpaquePointer {
  return unsafe test_fooptr4(len, p)
}

func call_test_fooptr5(_ len: CInt, _ p: OpaquePointer!) -> OpaquePointer {
  return unsafe test_fooptr5(len, p)
}

func call_test_voidptr(_ len: CInt, _ p: UnsafeMutableRawPointer!) -> UnsafeMutableRawPointer! {
  return unsafe test_voidptr(len, p)
}

func call_test_charptr(_ len: CInt, _ p: UnsafeMutablePointer<CChar>!) -> UnsafeMutablePointer<CChar>! {
  return unsafe test_charptr(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_charptr(_ p: UnsafeMutableRawBufferPointer) -> UnsafeMutablePointer<CChar>! {
  return unsafe test_charptr(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr(_ p: UnsafeRawBufferPointer) -> OpaquePointer! {
  return unsafe test_fooptr(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr2(_ p: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {
  return unsafe test_fooptr2(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr3(_ p: UnsafeRawBufferPointer) -> UnsafeRawBufferPointer {
  return unsafe test_fooptr3(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr4(_ p: UnsafeRawBufferPointer) -> OpaquePointer {
  return unsafe test_fooptr4(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr5(_ p: UnsafeRawBufferPointer) -> OpaquePointer {
  return unsafe test_fooptr5(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_voidptr(_ p: UnsafeMutableRawBufferPointer) -> UnsafeMutableRawPointer! {
  return unsafe test_voidptr(p)
}
