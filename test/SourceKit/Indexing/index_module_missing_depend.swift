// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/cycle-depend/A.swift -I %S/Inputs/cycle-depend -enable-source-import

// RUN: not %sourcekitd-test -req=index %t/A.swiftmodule -- %t/A.swiftmodule 2>&1 | %FileCheck %s

// FIXME: Report the reason we couldn't load a module.
// CHECK-DISABLED: error response (Request Failed): missing module dependency
// CHECK: error response (Request Failed): failed to load module
