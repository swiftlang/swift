/// Ensure inlined code in swiftmodules picks the right shadowed decl
/// relative to where they were inlined from, no matter if other imported
/// modules define more shadowers at the inlining site.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// REQUIRES: objc_interop

/// Reference build: Build libs and client for a normal build.
// RUN: %target-swift-frontend -emit-module %t/RootLib.swift -I %t \
// RUN:   -emit-module-path %t/RootLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/MiddleLib.swift -I %t \
// RUN:   -emit-module-path %t/MiddleLib.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/HighLib.swift -I %t \
// RUN:   -emit-module-path %t/HighLib.swiftmodule
// RUN: %target-swift-frontend -emit-sil -O %t/Client.swift -I %t > %t/Client.sil
// RUN: %FileCheck %s --input-file=%t/Client.sil

//--- module.modulemap
module RootLib {
  header "RootLib.h"
}
module ClangMiddleLib {
  header "ClangMiddleLib.h"
}

//--- RootLib.h
#include <stdio.h>
void ShadowedFunc() { // Never picked as it's shadowed by Swift's.
    printf("Clang\n");
}

void ShadowedFuncClang();

//--- ClangMiddleLib.h
#include "RootLib.h"
void ShadowedFuncClang();

//--- RootLib.swift
@_exported import RootLib
@_exported import ClangMiddleLib

public func ShadowedFunc() {
    print("Swift Root")
}

@inlinable
@inline(__always)
public func userFromRoot() {
    ShadowedFunc() // RootLib Swift one
}

@inlinable
@inline(__always)
public func clangUserFromRoot() {
    ShadowedFuncClang()
}

//--- MiddleLib.swift
@_exported import RootLib

public func ShadowedFunc() {
    print("Swift Middle")
}

@inlinable
@inline(__always)
public func userFromMiddle() {
    ShadowedFunc() // MiddleLib one
}

@inlinable
@inline(__always)
public func clangUserFromMiddle() {
    ShadowedFuncClang()
}

//--- HighLib.swift
@_exported import MiddleLib

@inlinable
@inline(__always)
public func userFromHigh() {
    ShadowedFunc() // MiddleLib one
}

@inlinable
@inline(__always)
public func explicitUserFromHigh() {
    RootLib.ShadowedFunc() // MRootLib.iddleLib one
}

@inlinable
@inline(__always)
public func clangUserFromHigh() {
    ShadowedFuncClang()
}

//--- Client.swift
import HighLib
import ClangMiddleLib

// RootLib call its own Swift ShadowedFunc.
userFromRoot()
// CHECK: [[ROOT_FUNC:%.*]] = function_ref @$s7RootLib12ShadowedFuncyyF : $@convention(thin) () -> ()
// CHECK: apply [[ROOT_FUNC]]() : $@convention(thin) () -> ()
explicitUserFromHigh()
// CHECK: apply [[ROOT_FUNC]]() : $@convention(thin) () -> ()
// CHECK-NOT: apply [[ROOT_FUNC]]() : $@convention(thin) () -> ()

// MiddleLib and HighLib call the last ShadowedFunc from MiddleLib.
userFromMiddle()
// CHECK: [[MIDDLE_FUNC:%.*]] = function_ref @$s9MiddleLib12ShadowedFuncyyF : $@convention(thin) () -> ()
// CHECK: apply [[MIDDLE_FUNC]]() : $@convention(thin) () -> ()
userFromHigh()
// CHECK: apply [[MIDDLE_FUNC]]() : $@convention(thin) () -> ()
// CHECK-NOT: apply [[MIDDLE_FUNC]]() : $@convention(thin) () -> ()

// All callers of the clang func use the merged one anyway.
clangUserFromRoot()
clangUserFromMiddle()
clangUserFromHigh()
// CHECK: [[CLANG_FUNC:%.*]] = function_ref @ShadowedFuncClang : $@convention(c) () -> ()
// CHECK: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
// CHECK: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
// CHECK: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
// CHECK-NOT: apply [[CLANG_FUNC]]() : $@convention(c) () -> ()
