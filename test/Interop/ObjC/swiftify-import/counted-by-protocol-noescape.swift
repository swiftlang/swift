// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// swift-ide-test doesn't currently trigger extension macro expansion, nor does it typecheck macro expansions, so dump macros with swift-frontend

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByProtocolSpan.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence %t/counted-by-protocol-noescape.swift -dump-macro-expansions  -swift-version 6 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module CountedByProtocolNoescapeClang {
    header "counted-by-protocol-noescape.h"
    export *
}

//--- Inputs/counted-by-protocol-noescape.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((__noescape__))
#define __lifetimebound __attribute__((__lifetimebound__))

__attribute__((swift_attr("@available(SwiftStdlib 6.2, *)")))
@protocol CountedByProtocolSpan
 - (void) simple:(int)len :(int * __counted_by(len) __noescape)p;
 - (void) shared:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len) __noescape)p2;
 - (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset) __noescape)p;
 - (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified __noescape)p;
 - (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull __noescape)p;
 - (void) nullable:(int)len :(int * __counted_by(len) _Nullable __noescape)p;

 - (int * __counted_by(len)) returnPointer:(int)len :(int)len2 :(int * __counted_by(len2) __lifetimebound) p;

 + (void) staticMethod:(int)len :(int * __counted_by(len) __noescape)p;
@end

// CHECK-LABEL: extension CountedByProtocolSpan {
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public func simple(_ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:         return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe simple(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:           }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p1: copy p1) @lifetime(p2: copy p2)
// CHECK-NEXT:     public func shared(_ len: Int32, _ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {
// CHECK-NEXT:         let _p1Count: some BinaryInteger = len
// CHECK-NEXT:           if p1.count < _p1Count || _p1Count < 0 {
// CHECK-NEXT:             fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:           }
// CHECK-NEXT:         let _p2Count: some BinaryInteger = len
// CHECK-NEXT:           if p2.count < _p2Count || _p2Count < 0 {
// CHECK-NEXT:             fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:           }
// CHECK-NEXT:         return unsafe p2.withUnsafeMutableBufferPointer { _p2Ptr in
// CHECK-NEXT:             return unsafe p1.withUnsafeMutableBufferPointer { _p1Ptr in
// CHECK-NEXT:                 return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)
// CHECK-NEXT:             }
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:         let _pCount: some BinaryInteger = len - offset
// CHECK-NEXT:         if p.count < _pCount || _pCount < 0 {
// CHECK-NEXT:             fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:         }
// CHECK-NEXT:         return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe complexExpr(len, offset, _pPtr.baseAddress!)
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public func nullUnspecified(_ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:         return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe nullUnspecified(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public func nonnull(_ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:         return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe nonnull(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public func nullable(_ p: inout MutableSpan<Int32>?) {
// CHECK-NEXT:         return { () in
// CHECK-NEXT:             if p == nil {
// CHECK-NEXT:                 unsafe nullable(Int32(exactly: p?.count ?? 0)!, nil)
// CHECK-NEXT:             } else {
// CHECK-NEXT:                 unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:                     return unsafe nullable(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress)
// CHECK-NEXT:                 }
// CHECK-NEXT:             }
// CHECK-NEXT:         }()
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(copy p) @lifetime(p: copy p)
// CHECK-NEXT:     public func returnPointer(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
// CHECK-NEXT:         return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe returnPointer(len, Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:           }, count: Int(len)), copying: ())
// CHECK-NEXT:     }
// CHECK-NEXT:     @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:     public static func staticMethod(_ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:         return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:             return unsafe staticMethod(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:         }
// CHECK-NEXT:     }
// CHECK-NEXT: }

__attribute__((swift_attr("@_SwiftifyImportProtocol(.method(name: \"func swiftAttr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)\", paramInfo: [.countedBy(pointer: .param(2), count: \"len\"), .nonescaping(pointer: .param(2))]), availability: [\"Span\": \"macOS 9999\", \"MutableSpan\": \"macOS 9999\"])")))
@protocol SwiftAttrProtocol
 - (void)swiftAttr:(int)len :(int *)p;
@end

// CHECK-LABEL: extension SwiftAttrProtocol {
// CHECK-NEXT:      @_alwaysEmitIntoClient @available(macOS 9999, *) @lifetime(p: copy p)
// CHECK-NEXT:      public func swiftAttr(_ p: inout MutableSpan<Int32>) {
// CHECK-NEXT:          return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
// CHECK-NEXT:              return unsafe swiftAttr(Int32(exactly: _pPtr.count)!, _pPtr.baseAddress!)
// CHECK-NEXT:          }
// CHECK-NEXT:      }
// CHECK-NEXT:  }

//--- counted-by-protocol-noescape.swift
import CountedByProtocolNoescapeClang
import Cxx

@available(macOS 9999, *)
@lifetime(p1: copy p1) @lifetime(p2: copy p2)
public func call(p1: inout MutableSpan<CInt>, p2: inout MutableSpan<CInt>, p3: inout MutableSpan<CInt>?, x: CInt, y: CInt, a: CountedByProtocolSpan, b: SwiftAttrProtocol) {
  a.simple(&p1)
  a.shared(x, &p1, &p2)
  a.complexExpr(x, y, &p1)
  a.nullUnspecified(&p1)
  a.nonnull(&p1)
  a.nullable(&p3)
  let r1: MutableSpan<CInt> = a.returnPointer(x, &p1)
  b.swiftAttr(&p1)
}
