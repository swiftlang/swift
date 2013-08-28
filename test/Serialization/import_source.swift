// RUN: not %swift -parse -I=%S/Inputs %s 2>%t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.txt

// CHECK: transparent_invalid.swift:3:10: error: expression does not type-check
// NEGATIVE-NOT: transparent_invalid.swift:7:10: error
import transparent_invalid

// NEGATIVE-NOT: import_source.swift:[[@LINE+1]]
var _ : Int = negate(5) // no-warning

// CHECK: import_source.swift:[[@LINE+1]]:22: error: 'CBool' is not a subtype of 'Int'
var _ : Int = negate(true) // no-warning
