import Foundation

var x = NSUTF8StringEncoding

var d : AnyGenerator<Int>

// RUN: %sourcekitd-test -req=cursor -pos=3:18 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-OVERLAY %s
// CHECK-OVERLAY:      source.lang.swift.ref.var.global
// CHECK-OVERLAY-NEXT: NSUTF8StringEncoding
// CHECK-OVERLAY-NEXT: s:v10Foundation20NSUTF8StringEncodingSu
// CHECK-OVERLAY-NEXT: UInt
// CHECK-OVERLAY-NEXT: <Declaration>public let NSUTF8StringEncoding: <Type usr="s:Su">UInt</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:13 %s -- %s %mcp_opt %clang-importer-sdk | FileCheck -check-prefix=CHECK-GENERATOR %s
// CHECK-GENERATOR-NOT: _AnyGeneratorBase
