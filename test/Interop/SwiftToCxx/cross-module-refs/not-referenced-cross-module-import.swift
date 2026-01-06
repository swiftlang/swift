// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/structs.swift -module-name Structs -emit-module -emit-module-path %t/Structs.swiftmodule -clang-header-expose-decls=all-public -emit-clang-header-path %t/structs.h

// RUN: %target-swift-frontend %s -module-name UsesStructs -I %t -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/uses-structs.h -clang-header-expose-module Structs=structs.h

// RUN: %FileCheck %s < %t/uses-structs.h
// RUN: %check-interop-cxx-header-in-clang(-I %t %t/uses-structs.h)

import Structs

// CHECK-NOT: structs.h
// CHECK: doesNotUseStructAPIs

fileprivate func usesStructsAPIsButNotExposed(_ x: StructSeveralI64) -> StructSeveralI64 {
    return Structs.passThroughStructSeveralI64(i: 0, x, j: 2)
}

public func doesNotUseStructAPIs() {
}
