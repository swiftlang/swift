// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-objc-types.swift -module-name UseObjCTy -typecheck -verify -emit-clang-header-path %t/UseObjCTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public

// RUN: %FileCheck %s < %t/UseObjCTy.h

// FIXME: remove once https://github.com/apple/swift/pull/60971 lands.
// RUN: echo "#include \"header.h\"" > %t/full-header.h
// RUN: cat %t/UseObjCTy.h >> %t/full-header.h

// RUN: %target-interop-build-clangxx -std=gnu++20 -fobjc-arc -c -x objective-c++-header %t/full-header.h -o %t/o.o

// REQUIRES: objc_interop

//--- header.h
#import <Foundation/Foundation.h>

@interface ObjCKlass
-(ObjCKlass * _Nonnull) init;
@end

@protocol ObjCProtocol
@required
- (void)method;
@end

@interface ObjCKlassConforming : NSObject<ObjCProtocol>
- (ObjCKlassConforming * _Nonnull) init;
- (void)method;
@end

typedef NS_OPTIONS(NSUInteger, ObjCKlassState) {
  ObjCKlassStateNormal  = 0,
};

//--- ObjCTest.apinotes
Name: ObjCTest
Tags:
- Name: ObjCKlassState
  SwiftName: ObjCKlass.State

//--- module.modulemap
module ObjCTest {
    header "header.h"
}

//--- use-objc-types.swift
import ObjCTest
import Foundation

@objc public class HasBlockField : NSObject {
    @objc var foo: ((ObjCKlass.State) -> Void)?
}

public func retObjClass() -> ObjCKlass {
    return ObjCKlass()
}

public func takeObjCClass(_ x: ObjCKlass) {
}

public func takeObjCClassInout(_ x: inout ObjCKlass) {
}

public func takeObjCClassNullable(_ x: ObjCKlass?) {
}

public func retObjClassNullable() -> ObjCKlass? {
    return nil
}

public func takeObjCProtocol(_ x: ObjCProtocol) {
}

public func retObjCProtocol() -> ObjCProtocol {
    return ObjCKlassConforming()
}

public func takeObjCProtocolNullable(_ x: ObjCProtocol?) {
}

public func retObjCProtocolNullable() -> ObjCProtocol? {
    return nil
}

public func retObjCClassArray() -> [ObjCKlass] {
    return []
}

public class KVOCookieMonster {
   public static func += (lhs: KVOCookieMonster, rhs: NSKeyValueObservation) {
      lhs.cookies.append(rhs)
   }

   private var cookies = Array<NSKeyValueObservation>()
}

// CHECK: @interface HasBlockField : NSObject
// CHECK: @property (nonatomic, copy) void (^ _Nullable foo)(ObjCKlassState);
// CHECK: @end
// CHECK: SWIFT_EXTERN id <ObjCProtocol> _Nonnull $s9UseObjCTy03retB9CProtocolSo0bE0_pyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // retObjCProtocol()
// CHECK-NEXT: #endif
// CHECK-NEXT: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_EXTERN id <ObjCProtocol> _Nullable $s9UseObjCTy03retB17CProtocolNullableSo0bE0_pSgyF(void) SWIFT_NOEXCEPT SWIFT_CALL; // retObjCProtocolNullable()
// CHECK-NEXT: #endif
// CHECK: ObjCKlass *_Nonnull $s9UseObjCTy03retB5ClassSo0B6CKlassCyF(void) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: ObjCKlass *_Nullable $s9UseObjCTy03retB13ClassNullableSo0B6CKlassCSgyF(void) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: void $s9UseObjCTy04takeB6CClassyySo0B6CKlassCF(ObjCKlass *_Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: void $s9UseObjCTy04takeB11CClassInoutyySo0B6CKlassCzF(ObjCKlass *_Nonnull __strong * _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: void $s9UseObjCTy04takeB14CClassNullableyySo0B6CKlassCSgF(ObjCKlass *_Nullable x) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-NEXT: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_EXTERN void $s9UseObjCTy04takeB9CProtocolyySo0bE0_pF(id <ObjCProtocol> _Nonnull x) SWIFT_NOEXCEPT SWIFT_CALL; // takeObjCProtocol(_:)
// CHECK-NEXT: #endif
// CHECK-NEXT: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_EXTERN void $s9UseObjCTy04takeB17CProtocolNullableyySo0bE0_pSgF(id <ObjCProtocol> _Nullable x) SWIFT_NOEXCEPT SWIFT_CALL; // takeObjCProtocolNullable(_:)
// CHECK-NEXT: #endif

// CHECK: inline const constexpr bool isUsableInGenericContext<ObjCKlass*> = true;
// CHECK-NEXT: template<>
// CHECK-NEXT: struct TypeMetadataTrait<ObjCKlass*> {
// CHECK-NEXT:   static SWIFT_INLINE_PRIVATE_HELPER void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:     return UseObjCTy::_impl::$sSo9ObjCKlassCMa(0)._0;
// CHECK-NEXT:   }
// CHECK-NEXT: };

// CHECK: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_INLINE_THUNK id <ObjCProtocol> _Nonnull retObjCProtocol() noexcept SWIFT_SYMBOL("s:9UseObjCTy03retB9CProtocolSo0bE0_pyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT:  return (__bridge_transfer id <ObjCProtocol>)(__bridge void *)UseObjCTy::_impl::$s9UseObjCTy03retB9CProtocolSo0bE0_pyF();
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_INLINE_THUNK id <ObjCProtocol> _Nullable retObjCProtocolNullable() noexcept SWIFT_SYMBOL("s:9UseObjCTy03retB17CProtocolNullableSo0bE0_pSgyF") SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: return (__bridge_transfer id <ObjCProtocol>)(__bridge void *)UseObjCTy::_impl::$s9UseObjCTy03retB17CProtocolNullableSo0bE0_pSgyF();
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: SWIFT_INLINE_THUNK ObjCKlass *_Nonnull retObjClass() noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: return (__bridge_transfer ObjCKlass *)(__bridge void *)UseObjCTy::_impl::$s9UseObjCTy03retB5ClassSo0B6CKlassCyF();

// CHECK: SWIFT_INLINE_THUNK ObjCKlass *_Nullable retObjClassNullable() noexcept SWIFT_SYMBOL({{.*}}) SWIFT_WARN_UNUSED_RESULT {
// CHECK-NEXT: return (__bridge_transfer ObjCKlass *)(__bridge void *)UseObjCTy::_impl::$s9UseObjCTy03retB13ClassNullableSo0B6CKlassCSgyF();

// CHECK: void takeObjCClass(ObjCKlass *_Nonnull x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK-NEXT: _impl::$s9UseObjCTy04takeB6CClassyySo0B6CKlassCF(x);

// CHECK: SWIFT_INLINE_THUNK void takeObjCClassInout(ObjCKlass *_Nonnull __strong & x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK-NEXT: _impl::$s9UseObjCTy04takeB11CClassInoutyySo0B6CKlassCzF(&x);

// CHECK: SWIFT_INLINE_THUNK void takeObjCClassNullable(ObjCKlass *_Nullable x) noexcept SWIFT_SYMBOL({{.*}}) {
// CHECK-NEXT: _impl::$s9UseObjCTy04takeB14CClassNullableyySo0B6CKlassCSgF(x);


// CHECK: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_INLINE_THUNK void takeObjCProtocol(id <ObjCProtocol> _Nonnull x) noexcept SWIFT_SYMBOL("s:9UseObjCTy04takeB9CProtocolyySo0bE0_pF") {
// CHECK-NEXT:   UseObjCTy::_impl::$s9UseObjCTy04takeB9CProtocolyySo0bE0_pF(x);
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if defined(__OBJC__)
// CHECK-NEXT: SWIFT_INLINE_THUNK void takeObjCProtocolNullable(id <ObjCProtocol> _Nullable x) noexcept SWIFT_SYMBOL("s:9UseObjCTy04takeB17CProtocolNullableyySo0bE0_pSgF") {
// CHECK-NEXT:   UseObjCTy::_impl::$s9UseObjCTy04takeB17CProtocolNullableyySo0bE0_pSgF(x);
// CHECK-NEXT: }
// CHECK-NEXT: #endif
