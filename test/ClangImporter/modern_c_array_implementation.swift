// @c @implementation and @objc @implementation should work with C arrays.

// REQUIRES: objc_interop
// REQUIRES: executable_test
// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ObjCAttrAllowsCArrays

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Modern projection visible
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/wrong.swift -I %t/include -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -verify-additional-prefix modern-c-array- -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-build-swift -emit-executable %t/impl.swift %t/main.swift -o %t/modern -I %t/include -module-cache-path %t/module-cache -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-codesign %t/modern
// RUN: %target-run %t/modern | %FileCheck %s

// Legacy projection visible
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/wrong.swift -I %t/include -target %target-has-inline-array-triple -verify-additional-prefix legacy-c-array- -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-build-swift -emit-executable %t/impl.swift %t/main.swift -o %t/legacy -I %t/include -module-cache-path %t/module-cache -target %target-has-inline-array-triple -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-codesign %t/legacy
// RUN: %target-run %t/legacy | %FileCheck %s

// CHECK: method 2 2 3 4
// CHECK-NEXT: property 7 2 3 4
// CHECK-NEXT: function 3 2 3 4

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface Impl : NSObject
- (void)bump:(int (*)[4])buf;
- (void)fetch:(int (*)[4])buf completionHandler:(void (^)(int (*)[4]))handler;
@property (nonatomic, nullable) int (*prop)[4];
@end

void cBump(int (*buf)[4]);

NS_ASSUME_NONNULL_END

//--- impl.swift
import CLib

#if MODERN_C_ARRAYS
typealias ImplArray = InlineArray<4, Int32>
#else
typealias ImplArray = (Int32, Int32, Int32, Int32)
#endif

@objc @implementation extension Impl {
  func bump(_ buf: UnsafeMutablePointer<ImplArray>) {
    UnsafeMutableRawPointer(buf).assumingMemoryBound(to: Int32.self).pointee += 1
  }

  func fetch(_ buf: UnsafeMutablePointer<ImplArray>) async -> UnsafeMutablePointer<ImplArray> {
    buf
  }

  var prop: UnsafeMutablePointer<ImplArray>?
}

@c @implementation func cBump(_ buf: UnsafeMutablePointer<ImplArray>) {
  UnsafeMutableRawPointer(buf).assumingMemoryBound(to: Int32.self).pointee += 1
}

//--- main.swift
import CLib

#if MODERN_C_ARRAYS
typealias CArray = InlineArray<4, Int32>
var array: CArray = [1, 2, 3, 4]
func show(_ label: String, _ a: CArray) {
  // FIXME: Workaround for optimize_none_with_opaque_values. Subscripting an
  // InlineArray fails SIL verification there (this is covered by other tests).
  a.span.withUnsafeBufferPointer { print(label, $0[0], $0[1], $0[2], $0[3]) }
}
#else
typealias CArray = (Int32, Int32, Int32, Int32)
var array: CArray = (1, 2, 3, 4)
func show(_ label: String, _ a: CArray) { print(label, a.0, a.1, a.2, a.3) }
#endif

let impl = Impl()
withUnsafeMutablePointer(to: &array) { pointer in
  impl.bump(pointer)
  show("method", pointer.pointee)

  impl.prop = pointer
  UnsafeMutableRawPointer(impl.prop!).assumingMemoryBound(to: Int32.self).pointee = 7
  show("property", impl.prop!.pointee)

  cBump(pointer)
  UnsafeMutableRawPointer(pointer).assumingMemoryBound(to: Int32.self).pointee = 3
  show("function", pointer.pointee)
}

//--- wrong.swift
import CLib

#if MODERN_C_ARRAYS
typealias Wrong = (Int32, Int32, Int32, Int32)
#else
typealias Wrong = InlineArray<4, Int32>
#endif

@objc @implementation extension Impl {
  func bump(_ buf: UnsafeMutablePointer<Wrong>) {}
  // expected-error@-1 {{instance method 'bump' of type '(UnsafeMutablePointer<Wrong>) -> ()'}}

  func fetch(_ buf: UnsafeMutablePointer<Wrong>) async -> UnsafeMutablePointer<Wrong> { buf }
  // expected-error@-1 {{instance method 'fetch' of type '(UnsafeMutablePointer<Wrong>) async -> UnsafeMutablePointer<Wrong>'}}

  var prop: UnsafeMutablePointer<Wrong>?
  // expected-error@-1 {{property 'prop' of type 'UnsafeMutablePointer<Wrong>?'}}
}

@c @implementation func cBump(_ buf: UnsafeMutablePointer<Wrong>) {}
// expected-error@-1 {{global function 'cBump' of type '(UnsafeMutablePointer<Wrong>) -> ()'}}
