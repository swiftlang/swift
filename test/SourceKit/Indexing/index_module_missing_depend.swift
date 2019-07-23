// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/../../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t %s -I %t -module-name A
// RUN: rm %t/empty.swiftmodule

// RUN: not %sourcekitd-test -req=index %t/A.swiftmodule -- %t/A.swiftmodule 2>&1 | %FileCheck %s

import empty

// FIXME: Report the reason we couldn't load a module.
// CHECK-DISABLED: error response (Request Failed): missing module dependency
// CHECK: error response (Request Failed): failed to load module
