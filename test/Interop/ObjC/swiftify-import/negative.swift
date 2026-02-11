// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// XFAIL: *
// Safe interop Protocol support removed for now

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t %t/test.swift -verify -Xcc -Wno-nullability-completeness
// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t %t/test.swift -dump-macro-expansions 2> %t/expansions.out
// RUN: %diff %t/expansions.out %t/expansions.expected

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

@protocol NoBounds
 - (void) ignore;
@end

@protocol NoYesNo
 - (void) ignore;
 - (void) simple:(int)len :(int * __counted_by(len))p;
 - (void) ignore2;
@end

@protocol YesNo
 - (void) simple:(int)len :(int * __counted_by(len))p;
 - (void) ignore;
@end

@protocol YesNoYes
 - (void) simple:(int)len :(int * __counted_by(len))p;
 - (void) ignore;
 - (void) simple2:(int)len :(int * __counted_by(len))p;
@end

//--- expansions.expected
@__swiftmacro_So05NoYesA023_SwiftifyImportProtocolfMe_.swift
------------------------------
extension NoYesNo {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func simple(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe simple(len, p.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So5YesNo23_SwiftifyImportProtocolfMe_.swift
------------------------------
extension YesNo {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func simple(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe simple(len, p.baseAddress!)
    }
}
------------------------------
@__swiftmacro_So05YesNoA023_SwiftifyImportProtocolfMe_.swift
------------------------------
extension YesNoYes {
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func simple(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe simple(len, p.baseAddress!)
    }
    /// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public
        func simple2(_ p: UnsafeMutableBufferPointer<Int32>) {
        let len = Int32(exactly: p.count)!
        return unsafe simple2(len, p.baseAddress!)
    }
}
------------------------------
//--- test.swift
import TestClang

@inlinable
public func call(p: UnsafeMutableBufferPointer<CInt>, x: NoBounds, y: NoYesNo, z: YesNo, å: YesNoYes) {
  x.ignore()
  y.ignore()
  y.simple(p)
  z.ignore()
  z.simple(p)
  å.ignore()
  å.simple(p)
  å.simple2(p)
}

//--- module.modulemap
module TestClang {
    header "test.h"
    export *
}


