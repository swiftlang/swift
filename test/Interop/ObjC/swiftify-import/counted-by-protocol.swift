// REQUIRES: swift_feature_SafeInteropWrappers

// swift-ide-test doesn't currently trigger extension macro expansion, nor does it typecheck macro expansions, so dump macros with swift-frontend

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByProtocol.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/counted-by-protocol.swift -dump-macro-expansions 2>&1 | %FileCheck %s

//--- Inputs/module.modulemap
module CountedByProtocolClang {
    header "counted-by-protocol.h"
    export *
}

//--- Inputs/counted-by-protocol.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

@protocol CountedByProtocol
 - (void) simple:(int)len :(int * __counted_by(len))p;
 - (void) shared:(int)len :(int * __counted_by(len))p1 :(int * __counted_by(len))p2;
 - (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset))p;
 - (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified)p;
 - (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull)p;
 - (void) nullable:(int)len :(int * __counted_by(len) _Nullable)p;
 - (int * __counted_by(len)) returnPointer:(int)len;

 + (void) staticMethod:(int)len :(int * __counted_by(len))p;
@end

// CHECK-LABEL: extension CountedByProtocol {
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func simple(_ p: UnsafeMutableBufferPointer<Int{{.*}}>) {
// CHECK-NEXT:          return unsafe simple(Int{{.*}}(exactly: p.count)!, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func shared(_ len: Int{{.*}}, _ p1: UnsafeMutableBufferPointer<Int{{.*}}>, _ p2: UnsafeMutableBufferPointer<Int{{.*}}>) {
// CHECK-NEXT:          let _p1Count: some BinaryInteger = len
// CHECK-NEXT:            if p1.count < _p1Count || _p1Count < 0 {
// CHECK-NEXT:              fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:            }
// CHECK-NEXT:          let _p2Count: some BinaryInteger = len
// CHECK-NEXT:            if p2.count < _p2Count || _p2Count < 0 {
// CHECK-NEXT:              fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:            }
// CHECK-NEXT:          return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func complexExpr(_ len: Int{{.*}}, _ offset: Int{{.*}}, _ p: UnsafeMutableBufferPointer<Int{{.*}}>) {
// CHECK-NEXT:          let _pCount: some BinaryInteger = len - offset
// CHECK-NEXT:            if p.count < _pCount || _pCount < 0 {
// CHECK-NEXT:              fatalError("bounds check failure when calling unsafe function")
// CHECK-NEXT:            }
// CHECK-NEXT:          return unsafe complexExpr(len, offset, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int{{.*}}>) {
// CHECK-NEXT:          return unsafe nullUnspecified(Int{{.*}}(exactly: p.count)!, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func nonnull(_ p: UnsafeMutableBufferPointer<Int{{.*}}>) {
// CHECK-NEXT:          return unsafe nonnull(Int{{.*}}(exactly: p.count)!, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func nullable(_ p: UnsafeMutableBufferPointer<Int{{.*}}>?) {
// CHECK-NEXT:          return unsafe nullable(Int{{.*}}(exactly: p?.count ?? 0)!, p?.baseAddress)
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient @_disfavoredOverload
// CHECK-NEXT:      public func returnPointer(_ len: Int{{.*}}) -> UnsafeMutableBufferPointer<Int{{.*}}> {
// CHECK-NEXT:          return unsafe UnsafeMutableBufferPointer<Int{{.*}}>(start: unsafe returnPointer(len), count: Int(len))
// CHECK-NEXT:      }
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public static func staticMethod(_ p: UnsafeMutableBufferPointer<Int32>) {
// CHECK-NEXT:          return unsafe staticMethod(Int{{.*}}(exactly: p.count)!, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:  }

__attribute__((swift_attr("@_SwiftifyImportProtocol(.method(name: \"func swiftAttr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)\", paramInfo: [.countedBy(pointer: .param(2), count: \"len\")]))")))
@protocol SwiftAttrProtocol
 - (void)swiftAttr:(int)len :(int *)p;
@end

// CHECK-LABEL: extension SwiftAttrProtocol {
// CHECK-NEXT:      @_alwaysEmitIntoClient
// CHECK-NEXT:      public func swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {
// CHECK-NEXT:          return unsafe swiftAttr(Int32(exactly: p.count)!, p.baseAddress!)
// CHECK-NEXT:      }
// CHECK-NEXT:  }

//--- counted-by-protocol.swift
import CountedByProtocolClang

@inlinable
public func call(p: UnsafeMutableBufferPointer<CInt>, x: CInt, y: CInt, a: CountedByProtocol, b: SwiftAttrProtocol) {
  a.simple(p)
  a.shared(x, p, p)
  a.complexExpr(x, y, p)
  a.nullUnspecified(p)
  a.nonnull(p)
  a.nullable(p)
  let r1: UnsafeMutableBufferPointer<CInt> = a.returnPointer(x)
  let r2 = a.returnPointer(x)
  let r3: UnsafeMutablePointer<CInt>? = r2 // make sure the original is the favored overload
  b.swiftAttr(p)
}
