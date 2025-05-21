/// Ensure inlined code in swiftmodules differentiates between shadowed Swift
/// and clang decls.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: objc_interop

/// Reference build: Build libs and client for a normal build.
// RUN: %target-swift-frontend -emit-module %t/RootLib.swift -I %t \
// RUN:   -emit-module-path %t/RootLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/MiddleLib.swift -I %t \
// RUN:   -emit-module-path %t/MiddleLib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t > %t/Client.sil
// RUN: %FileCheck %s --input-file=%t/Client.sil

//--- module.modulemap
module RootLib {
  header "RootLib.h"
}

//--- RootLib.h
#include <stdio.h>
void ShadowedFunc() {
    printf("Clang\n");
}

//--- RootLib.swift
@_exported import RootLib

@usableFromInline
internal func ShadowedFunc() {
    print("Swift")
}

@inlinable
@inline(__always)
public func swiftUser() {
    ShadowedFunc() // Swift one
}

//--- MiddleLib.swift

import RootLib

@inlinable
@inline(__always)
public func clangUser() {
    ShadowedFunc() // Clang one
}

//--- Client.swift

import RootLib
import MiddleLib

swiftUser()
// CHECK: [[SWIFT_FUNC:%.*]] = function_ref @$s7RootLib12ShadowedFuncyyF : $@convention(thin) () -> ()
// CHECK: apply [[SWIFT_FUNC]]() : $@convention(thin) () -> ()
// CHECK-NOT: apply [[SWIFT_FUNC]]() : $@convention(thin) () -> ()

clangUser()
// CHECK: [[CLANG_FUNC:%.*]] = function_ref @ShadowedFunc : $@convention(c) () -> ()
// CHECK: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
// CHECK-NOT: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
