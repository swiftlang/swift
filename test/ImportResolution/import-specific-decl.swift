// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/asdf.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -sdk "" -verify -verify-ignore-unrelated
// RUN: not %target-swift-frontend -typecheck %s -I %t -sdk "" 2>&1 | %FileCheck %s

import struct aeiou.U
import struct aeiou.E
import abcde
import asdf

var a : A // expected-error {{'A' is ambiguous for type lookup in this context}}
// CHECK: error: 'A' is ambiguous for type lookup in this context
// CHECK-DAG: abcde{{.+}}note: found this candidate
// CHECK-DAG: asdf{{.+}}note: found this candidate

var b : B = abcde.B()
var e : E = aeiou.E()

var u : U = aeiou.U()
var o : O // expected-error {{cannot find type 'O' in scope}}

var o2 = aeiou.O() // expected-error {{module 'aeiou' has no member named 'O'}}

