// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// XFAIL: *
// Safe interop Protocol support removed for now

// REQUIRES: foundation

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence %t/test.swift -verify -Xcc -Wno-nullability-completeness
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature LifetimeDependence %t/test.swift -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.h
#pragma once

#include <Foundation/Foundation.h>

// __counted_by definition inherited from Foundation.h
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

void foo(int len, int * __counted_by(len) p __noescape);
@protocol TestProtocol
 - (void) bar:(int * __noescape)p _Nullable:(int)asdf;
 - (void) simple:(int)len :(int * __counted_by(len) __noescape)p;
 - (void) shared:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len) __noescape)p2;
 - (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset) __noescape)p;
 - (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified __noescape)p;
 - (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull __noescape)p;
 - (void) nullable:(int)len :(int * __counted_by(len) _Nullable __noescape)p;
 - (int * __counted_by(len)) returnPointer:(int)len : (int * __counted_by(len) _Nullable) __lifetimebound p;
 - (void) mixedEscapability:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len))p2;
@end

@protocol StaticProtocol
 + (void) staticMethod:(int)len :(int * __counted_by(len) __noescape)p;
@end

@interface StaticInterface : NSObject <StaticProtocol> {}
@end

//--- expansions.expected
@__swiftmacro_So12TestProtocol015_SwiftifyImportB0fMe_.swift
------------------------------
extension TestProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public
        func simple(_ p: inout MutableSpan<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
          return unsafe simple(len, _pPtr.baseAddress!)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public
        func shared(_ p1: inout MutableSpan<Int32>, _ p2: inout MutableSpan<Int32>) {
        let len = Int32(exactly: p1.count)!
        if p2.count != len {
          fatalError("bounds check failure in shared: expected \(len) but got \(p2.count)")
        }
        return unsafe p2.withUnsafeMutableBufferPointer { _p2Ptr in
          return unsafe p1.withUnsafeMutableBufferPointer { _p1Ptr in
          return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)
          }
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public
        func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) {
        let _pCount = p.count
        if _pCount != len - offset {
          fatalError("bounds check failure in complexExpr: expected \(len - offset) but got \(_pCount)")
        }
        return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
          return unsafe complexExpr(len, offset, _pPtr.baseAddress!)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public
        func nullUnspecified(_ p: inout MutableSpan<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
          return unsafe nullUnspecified(len, _pPtr.baseAddress!)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public
        func nonnull(_ p: inout MutableSpan<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
          return unsafe nonnull(len, _pPtr.baseAddress!)
        }
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public
        func nullable(_ p: inout MutableSpan<Int32>?) {
        let len = Int32(exactly: p?.count ?? 0)!
        return { () in
            return if p == nil {
                unsafe nullable(len, nil)
              } else {
                unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
                  return unsafe nullable(len, _pPtr.baseAddress)
                }
              }
        }()
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public
        func returnPointer(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32> {
        let len = Int32(exactly: p?.count ?? 0)!
        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: { () in
                    return if p == nil {
                        unsafe returnPointer(len, nil)
                      } else {
                        unsafe p!.withUnsafeMutableBufferPointer { _pPtr in
                          return unsafe returnPointer(len, _pPtr.baseAddress)
                        }
                      }
                }(), count: Int(len)), copying: ())
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_disfavoredOverload public
        func mixedEscapability(_ p1: inout MutableSpan<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p1.count)!
        if p2.count != len {
          fatalError("bounds check failure in mixedEscapability: expected \(len) but got \(p2.count)")
        }
        return unsafe p1.withUnsafeMutableBufferPointer { _p1Ptr in
          return unsafe mixedEscapability(len, _p1Ptr.baseAddress!, p2.baseAddress!)
        }
    }
}
------------------------------
@__swiftmacro_So14StaticProtocol015_SwiftifyImportB0fMe_.swift
------------------------------
extension StaticProtocol {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
        static public func staticMethod(_ p: inout MutableSpan<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
          return unsafe staticMethod(len, _pPtr.baseAddress!)
        }
    }
}
------------------------------
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func foo(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    return unsafe p.withUnsafeMutableBufferPointer { _pPtr in
      return unsafe foo(len, _pPtr.baseAddress!)
    }
}
------------------------------
//--- test.swift
import TestClang

@inlinable
public func call(p: inout MutableSpan<CInt>, p2: inout MutableSpan<CInt>, p3: inout MutableSpan<CInt>?, a: TestProtocol) {
  a.simple(&p)
  a.shared(&p, &p2)
  a.complexExpr(2, 3, &p)
  a.nullUnspecified(&p)
  a.nonnull(&p)
  a.nullable(&p3)
  let _: MutableSpan<CInt> = a.returnPointer(&p3)
  StaticInterface.staticMethod(&p2)
  foo(&p)
}

//--- module.modulemap
module TestClang {
    header "test.h"
    export *
}
