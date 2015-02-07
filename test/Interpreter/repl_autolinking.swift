// RUN: rm -rf %t && mkdir %t
// RUN: sed -n -e '/MODULE_A_START$/,/MODULE_A_END$/ p' %s > %t/a.swift
// RUN: sed -n -e '/MODULE_B_START$/,/MODULE_B_END$/ p' %s > %t/b.swift
// RUN: sed -n -e '/REPL_START$/,/REPL_END$/ p' %s > %t/repl.swift
// RUN: %target-swiftc_driver -emit-library %t/a.swift -I %t -L %t -emit-module-path %t/ModuleA.swiftmodule -autolink-force-load -module-link-name ModuleA -module-name ModuleA -o %t/libModuleA.dylib
// RUN: %target-swiftc_driver -emit-library %t/b.swift -I %t -L %t -emit-module-path %t/ModuleB.swiftmodule -autolink-force-load -module-link-name ModuleB -module-name ModuleB -o %t/libModuleB.dylib
// RUN: %swift -repl -I %t -L %t < %t/repl.swift 2>&1 | FileCheck %s

// REQUIRES: swift_repl
// UNSUPPORTED: OS=linux-gnu

// This test checks that autolinking works in the REPL.

// MODULE_A_START
public func funcFromModuleA() -> String {
  return "<result from ModuleA>"
}
// MODULE_A_END

// MODULE_B_START
import ModuleA

public func funcFromModuleB() -> String {
  return funcFromModuleA() + "<result from ModuleB>"
}
// MODULE_B_END

// REPL_START
import ModuleB
funcFromModuleB()
funcFromModuleA()

import ModuleA
funcFromModuleA()
// REPL_END

// Don't insist on particular order, it is non-deterministic (since the output
// is stdout and stderr, mixed together).
//
// CHECK-DAG: "<result from ModuleA><result from ModuleB>"
// CHECK-DAG: error: use of unresolved identifier 'funcFromModuleA'
// CHECK-DAG: "<result from ModuleA>"

