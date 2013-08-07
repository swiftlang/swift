// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %swift -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %swift -emit-module -o %t %S/Inputs/asdf.swift
// RUN: %swift -emit-module -o %t -I=%t %S/Inputs/letters.swift
// RUN: %swift -parse %s -I=%t -sdk= -verify

import letters
import aeiou
import struct asdf.A
import struct abcde.A

var uA : A // expected-error {{'A' is ambiguous for type look up in this context}}
var uB : B = abcde.B()
var uC : C = letters.C()
var uD : D = asdf.D()
var uE : E = aeiou.E()
var uF : F = letters.F()

var qA1 : aeiou.A
var qA2 : asdf.A
var qA3 : abcde.A
