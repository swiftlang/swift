// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/FailedLoad/A.swift -module-name A -emit-module -emit-module-path %t/
// RUN: %target-build-swift %s -module-name FailedLoad -emit-module -I %t -emit-module-path %t/

// RUN: rm %t/A.swiftmodule

// RUN: not %target-swift-symbolgraph-extract -module-name FailedLoad -I %t -pretty-print -output-dir %t 2>&1 | %FileCheck %s

// CHECK-NOT: Emitting symbol graph for module file

import A

public struct Outer {
    public var Something: A.Inner
}
