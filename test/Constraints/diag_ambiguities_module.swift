// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/has_ambiguities.swift
// RUN: not %target-swift-frontend -parse %s -I %t 2>&1 | FileCheck %s

import has_ambiguities

maybeTrans(0) // expected-error{{ambiguous use of 'maybeTrans'}}
// CHECK: ambiguous use of 'maybeTrans'
// CHECK: maybeTrans(0)
// CHECK: found this candidate
// CHECK-NOT: transparent
// CHECK: maybeTrans(i: Int16)
// CHECK: found this candidate
// CHECK-NOT: transparent
// CHECK: maybeTrans(i: Int32)
