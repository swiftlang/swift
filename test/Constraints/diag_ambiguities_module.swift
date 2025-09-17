// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_ambiguities.swift
// RUN: not %target-swift-frontend -typecheck %s -I %t 2>&1 | %FileCheck %s

import has_ambiguities

maybeTrans(0) // expected-error{{ambiguous use of 'maybeTrans'}}
// CHECK: ambiguous use of 'maybeTrans'
// CHECK: maybeTrans(0)
// CHECK: found candidate with type '(Int16) -> ()' in module 'has_ambiguities'
// CHECK-NOT: transparent
// CHECK: maybeTrans(_ i: Int16)
// CHECK-NOT: transparent
// CHECK: maybeTrans(_ i: Int32)
// CHECK: found candidate with type '(Int32) -> ()' in module 'has_ambiquities'
