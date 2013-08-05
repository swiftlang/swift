// RUN: rm -f %t.*
// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift -emit-module -o %t %S/Inputs/ambiguous_left.swift
// RUN: %swift -emit-module -o %t %S/Inputs/ambiguous_right.swift
// RUN: %swift -emit-module -o %t -I %t %S/Inputs/ambiguous.swift

// RUN: %swift -parse -I=%t --serialize-diagnostics %t.dia %s -verify
// RUN: c-index-test -read-diagnostics %t.dia > %t.deserialized_diagnostics.txt 2>&1
// RUN: FileCheck --input-file=%t.deserialized_diagnostics.txt %s

import typealias swift.Int
import struct swift.Int

import class swift.Int // expected-error {{'Int' was imported as 'class', but is a struct}}
import func swift.Int // expected-error {{'Int' was imported as 'func', but is a struct}}
import var swift.Int // expected-error {{'Int' was imported as 'var', but is a struct}}

// CHECK: [[@LINE-4]]:14: error: 'Int' was imported as 'class', but is a struct
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE:.*import-specific-fixits.swift]]:[[@LINE-6]]:8 - [[FILE]]:[[@LINE-6]]:13): "struct"

import typealias swift.Enumerator // expected-error {{'Enumerator' was imported as 'typealias', but is a protocol}}
import struct swift.Enumerator // expected-error {{'Enumerator' was imported as 'struct', but is a protocol}}
import func swift.Enumerator // expected-error {{'Enumerator' was imported as 'func', but is a protocol}}

// CHECK: [[@LINE-4]]:18: error: 'Enumerator' was imported as 'typealias', but is a protocol
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE]]:[[@LINE-6]]:8 - [[FILE]]:[[@LINE-6]]:17): "protocol"

import class swift.Int64 // expected-error {{'Int64' was imported as 'class', but is a struct}}

// CHECK: [[@LINE-2]]:14: error: 'Int64' was imported as 'class', but is a struct
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE:.*import-specific-fixits.swift]]:[[@LINE-4]]:8 - [[FILE]]:[[@LINE-4]]:13): "struct"

import class swift.Bool // expected-error {{'Bool' was imported as 'class', but is a union}}

// CHECK: [[@LINE-2]]:14: error: 'Bool' was imported as 'class', but is a union
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE:.*import-specific-fixits.swift]]:[[@LINE-4]]:8 - [[FILE]]:[[@LINE-4]]:13): "union"

import struct swift.true // expected-error {{'true' was imported as 'struct', but is a variable}}

// CHECK: [[@LINE-2]]:15: error: 'true' was imported as 'struct', but is a variable
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE]]:[[@LINE-4]]:8 - [[FILE]]:[[@LINE-4]]:14): "var"

import struct swift.max // expected-error {{'max' was imported as 'struct', but is a function}}

// CHECK: [[@LINE-2]]:15: error: 'max' was imported as 'struct', but is a function
// CHECK-NEXT: Number FIXITs = 1
// CHECK-NEXT: FIXIT: ([[FILE]]:[[@LINE-4]]:8 - [[FILE]]:[[@LINE-4]]:14): "func"


import struct swift.min, swift.max // expected-error {{'min' was imported as 'struct', but is a function}} expected-error {{'max' was imported as 'struct', but is a function}}

// CHECK: [[@LINE-2]]:15: error: 'min' was imported as 'struct', but is a function
// CHECK-NEXT: Range: [[FILE]]:[[@LINE-3]]:21 [[FILE]]:[[@LINE-3]]:24
// CHECK-NEXT: Number FIXITs = 0

// CHECK: [[@LINE-6]]:26: error: 'max' was imported as 'struct', but is a function
// CHECK-NEXT: Range: [[FILE]]:[[@LINE-7]]:32 [[FILE]]:[[@LINE-7]]:35
// CHECK-NEXT: Number FIXITs = 0

import func ambiguous.funcOrVar // expected-error{{ambiguous name 'funcOrVar' in module 'ambiguous'}}
import var ambiguous.funcOrVar // expected-error{{ambiguous name 'funcOrVar' in module 'ambiguous'}}
import struct ambiguous.funcOrVar // expected-error{{ambiguous name 'funcOrVar' in module 'ambiguous'}}

import func ambiguous.someVar // expected-error{{ambiguous name 'someVar' in module 'ambiguous'}}
import var ambiguous.someVar // expected-error{{ambiguous name 'someVar' in module 'ambiguous'}}
import struct ambiguous.someVar // expected-error{{ambiguous name 'someVar' in module 'ambiguous'}}

import struct ambiguous.SomeStruct // expected-error{{ambiguous name 'SomeStruct' in module 'ambiguous'}}
import typealias ambiguous.SomeStruct // expected-error{{ambiguous name 'SomeStruct' in module 'ambiguous'}}
import class ambiguous.SomeStruct // expected-error{{ambiguous name 'SomeStruct' in module 'ambiguous'}}

import func ambiguous.overloadedFunc // no-warning
