// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//--- Main.swift
import FromClang // NOTE: line offset = -4

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_LifetimeDependence

// The macro-generated interface we're looking up source info for
// (this is more so for documentation than checking correctness)
//
// INTERFACE:      @_alwaysEmitIntoClient @_disfavoredOverload public func hasBufferOverload(_ p: UnsafeMutableBufferPointer<Int32>)
// INTERFACE:      @{{_?}}lifetime(p: copy p)
// INTERFACE-NEXT: @_alwaysEmitIntoClient @_disfavoredOverload public func hasSpanOverload(_ p: inout MutableSpan<Int32>)
// RUN: %target-swift-ide-test \
// RUN:   -print-module -module-to-print=FromClang -source-filename=x \
// RUN:   -plugin-path %swift-plugin-dir -I %t/Inputs \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN: | %FileCheck %t/Main.swift --check-prefix INTERFACE

@inlinable
public func callWithBufferPtr(_ p: UnsafeMutableBufferPointer<CInt>) {
  hasBufferOverload(p)
// RUN: %sourcekitd-test -req=cursor -pos=%(line-4):3 %t/Main.swift -- -I %t/Inputs %t/Main.swift \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN: | %FileCheck %t/Inputs/from-clang.h --check-prefix BUFFER-OVERLOAD
}

@available(visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@lifetime(p: copy p)
@inlinable
public func callReturnLifetimeBound(_ p: inout MutableSpan<CInt>) {
  hasSpanOverload(p)
// RUN: %sourcekitd-test -req=cursor -pos=%(line-4):3 %t/Main.swift -- -I %t/Inputs %t/Main.swift \
// RUN:   -enable-experimental-feature SafeInteropWrappers \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN: | %FileCheck %t/Inputs/from-clang.h --check-prefix SPAN-OVERLOAD
}

//--- Inputs/module.modulemap
module FromClang {
    header "from-clang.h"
    export *
}

//--- Inputs/from-clang.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

void hasBufferOverload(int len, int * __counted_by(len) p);
// BUFFER-OVERLOAD:      source.lang.swift.ref.function.free
// BUFFER-OVERLOAD-SAME: from-clang.h:[[@LINE-2]]

void hasSpanOverload(int len, int * __counted_by(len) __noescape p);
// SPAN-OVERLOAD:      source.lang.swift.ref.function.free
// SPAN-OVERLOAD-SAME: from-clang.h:[[@LINE-2]]
