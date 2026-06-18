// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Regression test for making sure we handle the hacky synthetic typedefs that
// clang generates when applying bounds attributes to typedefs.

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __single __attribute__((__single__))

typedef struct foo *fooptr_t;
typedef void *voidptr_t;
typedef char *charptr_t;

// expected-expansion@+8:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-error@2{{cannot find type 'fooptr_t __single' in scope}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_fooptr(_ p: UnsafeRawBufferPointer) -> `fooptr_t __single`! {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_fooptr(len, OpaquePointer(p.baseAddress))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
fooptr_t __single test_fooptr(int len, fooptr_t __sized_by(len) p);

// expected-expansion@+8:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-error@2{{cannot find type 'voidptr_t __single' in scope}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_voidptr(_ p: UnsafeMutableRawBufferPointer) -> `voidptr_t __single`! {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_voidptr(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
voidptr_t __single test_voidptr(int len, voidptr_t __sized_by(len) p);

// expected-expansion@+8:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-error@2{{cannot find type 'charptr_t __single' in scope}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func test_charptr(_ p: UnsafeMutableRawBufferPointer) -> `charptr_t __single`! {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe test_charptr(len, p.baseAddress.assumingMemoryBound(to: CChar.self))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
charptr_t __single test_charptr(int len, charptr_t __sized_by(len) p);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: fbef1dddc85a7d7d26154012a00dae11d29ec59d69b2f7839a9fc095943ee05f
import Test


// expected-error@+1{{cannot find type 'fooptr_t __single' in scope}}
func call_test_fooptr(_ len: Int32, _ p: OpaquePointer!) -> `fooptr_t __single`! {
  return unsafe test_fooptr(len, p)
}

// expected-error@+1{{cannot find type 'voidptr_t __single' in scope}}
func call_test_voidptr(_ len: Int32, _ p: UnsafeMutableRawPointer!) -> `voidptr_t __single`! {
  return unsafe test_voidptr(len, p)
}

// expected-error@+1{{cannot find type 'charptr_t __single' in scope}}
func call_test_charptr(_ len: Int32, _ p: UnsafeMutablePointer<CChar>!) -> `charptr_t __single`! {
  return unsafe test_charptr(len, p)
}

// expected-error@+1{{cannot find type 'charptr_t __single' in scope}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_charptr(_ p: UnsafeMutableRawBufferPointer) -> `charptr_t __single`! {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'Int32'}}
  return unsafe test_charptr(p)
}

// expected-error@+1{{cannot find type 'fooptr_t __single' in scope}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_fooptr(_ p: UnsafeRawBufferPointer) -> `fooptr_t __single`! {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeRawBufferPointer' to expected argument type 'Int32'}}
  return unsafe test_fooptr(p)
}

// expected-error@+1{{cannot find type 'voidptr_t __single' in scope}}
@_alwaysEmitIntoClient @_disfavoredOverload public func call_test_voidptr(_ p: UnsafeMutableRawBufferPointer) -> `voidptr_t __single`! {
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'Int32'}}
  return unsafe test_voidptr(p)
}
