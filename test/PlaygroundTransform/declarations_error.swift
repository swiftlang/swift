// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -typecheck -swift-version 5 -playground -diagnostic-style llvm %t/main.swift 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -swift-version 6 -playground -diagnostic-style llvm %t/main.swift 2>&1 | %FileCheck %s

// CHECK: error: no such module
import Nonexistent_Module
// CHECK-NOT: error
import Another_Nonexistent_Module
