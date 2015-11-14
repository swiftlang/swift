// RUN: rm -rf %t && mkdir %t
// RUN: touch %t/Corrupted_Module.swiftmodule
// RUN: not %target-repl-run-simple-swift -I %t 2> %t/stderr.txt | FileCheck %s
// RUN: FileCheck -check-prefix CHECK-ERROR %s < %t/stderr.txt

// REQUIRES: objc_interop
// REQUIRES: swift_repl

"start"
// CHECK-LABEL: String = "start"{{$}}

import MapKit

// Use an inline function that references other inline functions.
let x = MKMapRectMake(0.0, 1.0, 2.0, 3.0)
// CHECK-NEXT: x : MKMapRect

import Nonexistant_Module_Name
// CHECK-ERROR: error: no such module 'Nonexistant_Module_Name'

import SpriteKit.Nonexistant_Submodule
// CHECK-ERROR: error: no such module

SKScene()
// CHECK-ERROR: error: use of unresolved identifier 'SKScene'

// Use another inline function that references other inline functions.
MKMapRectIsNull(x)
// CHECK-NEXT: = false{{$}}

"end"
// CHECK-NEXT: String = "end"{{$}}


import Corrupted_Module
// CHECK-ERROR: error: malformed module file

"unreached"
// CHECK-NOT: unreached
