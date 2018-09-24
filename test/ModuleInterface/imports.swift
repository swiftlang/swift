// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/empty.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-interface-path - -emit-module -o /dev/null %s %S/Inputs/imports-other.swift -I %S/Inputs/imports-clang-modules/ -I %t -verify | %FileCheck %s


@_exported import empty
import B.B2
import func C.c // expected-warning {{scoped imports are not yet supported in textual module interfaces}}
import D

// CHECK-NOT: import
// CHECK: {{^}}import A{{$}}
// CHECK-NEXT: {{^}}import B{{$}}
// CHECK-NEXT: {{^}}import B.B2{{$}}
// CHECK-NEXT: {{^}}import B.B3{{$}}
// CHECK-NEXT: {{^}}import C/*.c*/{{$}}
// CHECK-NEXT: {{^}}import D{{$}}
// CHECK-NEXT: {{^}}@_exported import empty{{$}}
// CHECK-NOT: import
