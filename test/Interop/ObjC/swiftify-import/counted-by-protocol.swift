// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// XFAIL: *
// Safe interop Protocol support removed for now

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByProtocol.swiftmodule -I %t/Inputs -enable-experimental-feature StabilizedSafeInteropWrappers %t/counted-by-protocol.swift -verify -Xcc -Wno-nullability-completeness
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/CountedByProtocol.swiftmodule -I %t/Inputs -enable-experimental-feature StabilizedSafeInteropWrappers %t/counted-by-protocol.swift -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- Inputs/module.modulemap
module CountedByProtocolClang {
    header "counted-by-protocol.h"
    export *
}

//--- Inputs/counted-by-protocol.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

int foo(int len, int * __counted_by(len) p);
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

__attribute__((swift_attr("@_SwiftifyImportProtocol(.method(signature: \"func swiftAttr(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!)\", paramInfo: [.countedBy(pointer: .param(2), count: \"len\")]))")))
@protocol SwiftAttrProtocol
 - (void)swiftAttr:(int)len :(int *)p;
@end

//--- expansions.expected
@__swiftmacro_So17CountedByProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension CountedByProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func simple(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe simple(len, p.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p1.count)!
        if p2.count != len {
          fatalError("bounds check failure in shared: expected \(len) but got \(p2.count)")
        }
        return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
        let _pCount = p.count
        if _pCount != len - offset {
          fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
        }
        return unsafe complexExpr(len, offset, p.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe nullUnspecified(len, p.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe nonnull(len, p.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {
        let len = Int32(exactly: unsafe p?.count ?? 0)!
        return unsafe nullable(len, p?.baseAddress)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
        return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload
        static public func staticMethod(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe staticMethod(len, p.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So17SwiftAttrProtocol015_SwiftifyImportC0fMe_.swift
------------------------------
extension SwiftAttrProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func swiftAttr(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe swiftAttr(len, p.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func foo(_ p: UnsafeMutableBufferPointer<Int32>) -> Int32 {
    let len = Int32(exactly: p.count)!
    return unsafe foo(len, p.baseAddress!)
}
------------------------------
//--- counted-by-protocol.swift
import CountedByProtocolClang

@inlinable
public func call(p: UnsafeMutableBufferPointer<CInt>, x: CInt, y: CInt, a: CountedByProtocol, b: SwiftAttrProtocol) {
  a.simple(p)
  a.shared(p, p)
  a.complexExpr(x, y, p)
  a.nullUnspecified(p)
  a.nonnull(p)
  a.nullable(p)
  let _: UnsafeMutableBufferPointer<CInt> = a.returnPointer(x)
  let r2 = a.returnPointer(x)
  let _: UnsafeMutablePointer<CInt>? = r2 // make sure the original is the favored overload
  b.swiftAttr(p)
  let _ = foo(p)
}
