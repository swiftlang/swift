// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/use-objc-types.swift -typecheck -module-name UseObjCTy -emit-clang-header-path %t/UseObjCTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-public-decls

// RUN: %FileCheck %s < %t/UseObjCTy.h

// FIXME: remove once https://github.com/apple/swift/pull/60971 lands.
// RUN: echo "#include \"header.h\"" > %t/full-cxx-swift-cxx-bridging.h
// RUN: cat %t/UseObjCTy.h >> %t/full-cxx-swift-cxx-bridging.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/full-cxx-swift-cxx-bridging.h -o %t/o.o

// REQUIRES: objc_interop

//--- header.h

@interface ObjCKlass
-(ObjCKlass * _Nonnull) init;
@end

//--- module.modulemap
module ObjCTest {
    header "header.h"
}

//--- use-objc-types.swift
import ObjCTest

public func retObjClass() -> ObjCKlass {
    return ObjCKlass()
}

public func takeObjCClass(_ x: ObjCKlass) {
}

// FIXME: Test inout.

// CHECK: ObjCKlass *_Nonnull retObjClass() noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: return _impl::$s9UseObjCTy03retB5ClassSo0B6CKlassCyF();

// CHECK: void takeObjCClass(ObjCKlass *_Nonnull x) noexcept {
// CHECK-NEXT: return _impl::$s9UseObjCTy04takeB6CClassyySo0B6CKlassCF(x);
