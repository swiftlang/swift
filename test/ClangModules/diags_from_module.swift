// RUN: not %target-swift-frontend -parse %s -F %S/Inputs/frameworks -Xcc -D -Xcc FOO 2> %t.err.txt
// RUN: FileCheck -input-file=%t.err.txt %s

// XFAIL: linux

import Module

// CHECK: Another.h:2:4: error: Module should have been built without -DFOO
// CHECK: Sub2.h:2:9: error: could not build module 'Another'
// CHECK: diags_from_module.swift:[[@LINE-4]]:8: error: could not build Objective-C module 'Module'

// RUN: %target-swift-frontend -parse %s -F %S/Inputs/frameworks 2> %tw.err.txt
// RUN: FileCheck -input-file=%tw.err.txt %s -check-prefix=CHECK-WARN

// CHECK-WARN: Sub2.h:7:2: warning: here is some warning about something
// FIXME: show the clang warning: <module-includes>:1:1: warning: umbrella header for module 'Module' does not include header 'NotInModule.h' [-Wincomplete-umbrella]
