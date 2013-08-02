// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %swift -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %swift -parse %s -I=%t -sdk= -verify

import struct aeiou.U
import abcde

var a : A = abcde.A()
var b : B = abcde.B()

var u : U = aeiou.U()
var o : O // expected-error {{use of undeclared type 'O'}}

// FIXME: This should be an error because of the access path.
var o2 = aeiou.O() // no-warning
