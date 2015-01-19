// RUN: rm -rf %t && mkdir %t
// RUN: cp %s %t/main.swift
// RUN: not %target-swift-frontend -parse -playground %t/main.swift 2>&1 | FileCheck %s

// CHECK: error: no such module
import Nonexistent_Module
// CHECK-NOT: error
import Another_Nonexistent_Module
