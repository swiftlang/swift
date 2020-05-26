// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/empty.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/emptyButWithLibraryEvolution.swiftmodule %S/../Inputs/empty.swift -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s %S/Inputs/imports-other.swift -I %S/Inputs/imports-clang-modules/ -I %t -verify -swift-version 5 -enable-library-evolution | %FileCheck -implicit-check-not BAD %s


@_exported import empty // expected-warning {{module 'empty' was not compiled with library evolution support; using it means binary compatibility for 'imports' can't be guaranteed}}
@_exported import emptyButWithLibraryEvolution
import B.B2
import func C.c // expected-warning {{scoped imports are not yet supported in module interfaces}}
import D
@_implementationOnly import Secret_BAD

@_implementationOnly import NotSoSecret // expected-note {{imported as implementation-only here}}
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}}

// CHECK-NOT: import
// CHECK: {{^}}import A{{$}}
// CHECK-NEXT: {{^}}import B{{$}}
// CHECK-NEXT: {{^}}import B.B2{{$}}
// CHECK-NEXT: {{^}}import B.B3{{$}}
// CHECK-NEXT: {{^}}import C/*.c*/{{$}}
// CHECK-NEXT: {{^}}import D{{$}}
// CHECK-NEXT: {{^}}import NotSoSecret{{$}}
// CHECK-NEXT: {{^}}import NotSoSecret2{{$}}
// CHECK-NEXT: {{^}}import Swift{{$}}
// CHECK-NEXT: {{^}}@_exported import empty{{$}}
// CHECK-NEXT: {{^}}@_exported import emptyButWithLibraryEvolution{{$}}
// CHECK-NOT: import
