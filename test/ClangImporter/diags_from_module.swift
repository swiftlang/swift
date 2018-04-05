// RUN: not %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -Xcc -D -Xcc FOO 2> %t.err.txt
// RUN: %FileCheck -input-file=%t.err.txt %s

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks 2> %t.warn.txt
// RUN: %FileCheck -input-file=%t.warn.txt %s -check-prefix=CHECK-WARN

// RUN: %target-swift-frontend -typecheck %s -F %S/Inputs/frameworks -Xcc -Wno-#warnings 2> %t.nowarn.txt
// RUN: %FileCheck -input-file=%t.nowarn.txt %s -check-prefix=CHECK-NO-WARN -allow-empty

// XFAIL: linux

import Module

// CHECK: Another.h:2:4: error: Module should have been built without -DFOO
// CHECK: Sub2.h:2:9: error: could not build module 'Another'
// CHECK: diags_from_module.swift:[[@LINE-4]]:8: error: could not build Objective-C module 'Module'

// CHECK-WARN: Sub2.h:7:2: warning: here is some warning about something
// FIXME: show the clang warning: <module-includes>:1:1: warning: umbrella header for module 'Module' does not include header 'NotInModule.h' [-Wincomplete-umbrella]

// CHECK-NO-WARN-NOT: warning about something
