// RUN: rm -rf %t
// RUN: mkdir -p %t

// RUN: %target-swift-frontend -typecheck %s -enable-source-import -I %S/Inputs -sdk "" -verify -show-diagnostics-after-fatal
// RUN: not %target-swift-frontend -typecheck %s -I %S/Inputs -sdk "" -show-diagnostics-after-fatal 2>&1 | %FileCheck %s -check-prefix=CHECK-NO-SOURCE-IMPORT

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/abcde.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/aeiou.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/asdf.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t %S/Inputs/letters.swift
// RUN: %target-swift-frontend -typecheck %s -I %t -sdk "" -verify -show-diagnostics-after-fatal

// RUN: %target-swift-ide-test -source-filename %s -print-module-imports -module-to-print=letters -I %t | %FileCheck %s -check-prefix=CHECK-IMPORTS

// CHECK-NO-SOURCE-IMPORT: no such module 'letters'
// CHECK-NO-SOURCE-IMPORT: no such module 'abcde'
// CHECK-NO-SOURCE-IMPORT: no such module 'asdf'
import letters
import abcde
import struct asdf.S

var qA : letters.A // expected-error {{ambiguous type name 'A' in module 'letters'}}
var qB : letters.B = abcde.B()
var qC : letters.C = letters.C(b: qB)
var qD : letters.D = asdf.D()
var qE : letters.E = aeiou.E()
var qF : letters.F = letters.F()

letters.abcde.A // expected-error{{module 'letters' has no member named 'abcde'}}
letters.aeiou.A // expected-error{{module 'letters' has no member named 'aeiou'}}
letters.asdf.A // expected-error{{module 'letters' has no member named 'asdf'}}

var uA : A // expected-error {{'A' is ambiguous for type lookup in this context}}
var uB : B = abcde.B()
var uC : C // expected-error {{'C' is ambiguous for type lookup in this context}}
var uD : D // expected-error {{'D' is ambiguous for type lookup in this context}}
var uE : E // expected-error {{'E' is ambiguous for type lookup in this context}}
var uF : F = letters.F()

var qA1 : abcde.A // okay
var qA2 : aeiou.A // okay
var qA3 : asdf.A // expected-error {{no type named 'A' in module 'asdf'}}
var qD1 : asdf.D // okay
var qS1 : asdf.S // okay


// CHECK-IMPORTS: letters:
// CHECK-IMPORTS-DAG: 	abcde
// CHECK-IMPORTS-DAG: 	aeiou
// CHECK-IMPORTS-DAG: 	aeiou.E
// CHECK-IMPORTS-DAG: 	asdf.D
// CHECK-IMPORTS-NOT:   asdf.S
// CHECK-IMPORTS-DAG: asdf:
// CHECK-IMPORTS-DAG: aeiou:
// CHECK-IMPORTS-DAG: aeiou:
// CHECK-IMPORTS-DAG: abcde:
