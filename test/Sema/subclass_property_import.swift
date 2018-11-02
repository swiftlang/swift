// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/rdar32973206_a.swiftmodule %S/Inputs/rdar32973206_a.swift
// RUN: %target-swift-frontend -I %t -emit-module -o %t/rdar32973206_b.swiftmodule %S/Inputs/rdar32973206_b.swift
// RUN: %target-swift-frontend -I %t -emit-module -o %t/rdar32973206_c.swiftmodule %S/Inputs/rdar32973206_c.swift
// RUN: %target-swift-frontend -I %t -emit-sil -verify %s | %FileCheck %s

import rdar32973206_a
import rdar32973206_b
import rdar32973206_c

// CHECK: A.foo
let _ = A.foo

// CHECK: B.foo
let _ = B.foo

// CHECK: B.foo
let _ = C.foo
