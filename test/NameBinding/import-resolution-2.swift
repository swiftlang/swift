// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/asdf.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/letters.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -sdk "" -verify

import letters
import aeiou
import struct asdf.A
import struct abcde.A

var uA : A // expected-error {{'A' is ambiguous for type lookup in this context}}
var uB : B = abcde.B()
var uC : C = letters.C(b: uB)
var uD : D = asdf.D()
var uE : E = aeiou.E()
var uF : F = letters.F()

var qA1 : aeiou.A
var qA2 : asdf.A
var qA3 : abcde.A

var uS : S // expected-error {{use of undeclared type 'S'}}
var qS1 : letters.S // expected-error {{no type named 'S' in module 'letters'}}
var qS2 : asdf.S // expected-error {{no type named 'S' in module 'asdf'}}
// but...!
letters.consumeS(letters.myS)
