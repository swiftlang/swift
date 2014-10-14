// RUN: rm -rf %t.mcp
// RUN: not %swift -parse %s -F %S/Inputs/frameworks -module-cache-path %t.mcp -Xcc -D -Xcc FOO 2> %t.err.txt
// RUN: FileCheck -input-file=%t.err.txt %s

import Module

// CHECK: Another.h:2:4: error: Module should have been built without -DFOO
// CHECK: Sub2.h:1:9: error: could not build module 'Another'
// CHECK: diags_from_module.swift:[[@LINE-4]]:8: error: could not build Objective-C module 'Module'
