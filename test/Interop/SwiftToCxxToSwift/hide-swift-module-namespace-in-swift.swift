// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/swiftMod.swift -typecheck -module-name SwiftMod -emit-clang-header-path %t/swiftMod.h -I %t -enable-experimental-cxx-interop -Xcc -DFIRSTPASS

// RUN: %FileCheck %s < %t/swiftMod.h

// RUN: %target-swift-frontend -typecheck %t/swiftMod.swift -typecheck -module-name SwiftMod -emit-clang-header-path %t/swiftMod2.h -I %t -enable-experimental-cxx-interop -Xcc -DSWIFT_CXX_INTEROP_HIDE_SWIFT_ERROR

// RUN: %check-interop-cxx-header-in-clang(%t/swiftMod2.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY  -Wno-error)

// XFAIL: OS=linux-android, OS=linux-androideabi

//--- header.h
#ifndef FIRSTPASS
#include "swiftMod.h"
#endif

//--- module.modulemap
module SwiftToCxxTest {
    header "header.h"
    requires cplusplus
}

//--- swiftMod.swift
import SwiftToCxxTest

@_expose(Cxx)
public func testFunction() -> String {
    let arr = Swift.Array<Int>()
    let rng = Swift.SystemRandomNumberGenerator()
    return ""
}

// CHECK: namespace Swift __attribute__((swift_private)) {
// CHECK: namespace SwiftMod __attribute__((swift_private)) {
// CHECK-NOT: namespace Swift {
