// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -module-cache-path %t -enable-objc-interop -typecheck %s -F %S/Inputs/frameworks -Xcc -D -Xcc FOO -o /dev/null 2>&1 | %FileCheck %s

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-cache-path %t -enable-objc-interop -typecheck %s -F %S/Inputs/frameworks -o /dev/null 2>&1 | %FileCheck %s -check-prefix CHECK-WARN

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-cache-path %t -enable-objc-interop -typecheck %s -F %S/Inputs/frameworks -Xcc -Wno-#warnings -o /dev/null 2>&1 | %FileCheck -check-prefix CHECK-NO-WARN -allow-empty %s

import Module

// CHECK: Another.h:2:4: error: Module should have been built without -DFOO
// CHECK: Sub2.h:2:9: error: could not build module 'Another'
// CHECK: diags_from_module.swift:[[@LINE-4]]:8: error: could not build Objective-C module 'Module'

// CHECK-WARN: Sub2.h:7:2: warning: here is some warning about something
// CHECK-WARN: <module-includes>:1:1: warning: umbrella header for module 'Module' does not include header 'NotInModule.h'
// FIXME: show [-Wincomplete-umbrella]

// CHECK-NO-WARN-NOT: warning about something
