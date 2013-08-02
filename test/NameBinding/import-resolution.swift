// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %swift -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %swift -emit-module -o %t -I=%t %S/Inputs/letters.swift
// RUN: %swift -parse %s -I=%t -sdk= -verify

import letters
import abcde

var qA : letters.A // expected-error {{ambiguous type name 'A' in module 'letters'}}
var qB : letters.B
var qC : letters.C

letters.abcde.A // expected-error{{'module<letters>' does not have a member named 'abcde'}}
letters.aeiou.A // expected-error{{'module<letters>' does not have a member named 'aeiou'}}

var uA : A // expected-error {{'A' is ambiguous for type look up in this context}}
var uB : B
var uC : C // expected-error {{'C' is ambiguous for type look up in this context}}

var qA1 : abcde.A // okay
var qA2 : aeiou.A // okay
