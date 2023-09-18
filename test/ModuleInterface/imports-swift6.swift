/// Swift 6 variant to imports.swift. Both can be reintegrated once
/// -swift-version 6 is accepted by release compilers.
// REQUIRES: asserts

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -emit-module -o %t/empty.swiftmodule %S/../Inputs/empty.swift
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -emit-module -o %t/emptyButWithLibraryEvolution.swiftmodule %S/../Inputs/empty.swift -enable-library-evolution

/// Swift 6 variant.
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s %S/Inputs/imports-other.swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %S/Inputs/imports-clang-modules/ -I %t -verify -swift-version 6
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -I %S/Inputs/imports-clang-modules/ -I %t
// RUN: %FileCheck -implicit-check-not BAD -check-prefix CHECK-6 %s < %t.swiftinterface

@_exported import empty // expected-warning {{module 'empty' was not compiled with library evolution support; using it means binary compatibility for 'main' can't be guaranteed}}
@_exported import emptyButWithLibraryEvolution
import B.B2
import func C.c // expected-warning {{scoped imports are not yet supported in module interfaces}}
import D
@_implementationOnly import Secret_BAD

@_implementationOnly import NotSoSecret // expected-note {{imported as implementation-only here}}
import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}}

// CHECK-6-NOT: import
// CHECK-6: {{^}}public import A{{$}}
// CHECK-6-NEXT: {{^}}public import B{{$}}
// CHECK-6-NEXT: {{^}}public import B.B2{{$}}
// CHECK-6-NEXT: {{^}}public import B.B3{{$}}
// CHECK-6-NEXT: {{^}}public import C/*.c*/{{$}}
// CHECK-6-NEXT: {{^}}public import D{{$}}
// CHECK-6-NEXT: {{^}}public import NotSoSecret{{$}}
// CHECK-6-NEXT: {{^}}public import NotSoSecret2{{$}}
// CHECK-6-NEXT: {{^}}public import Swift{{$}}
// CHECK-6-NEXT: {{^}}@_exported public import empty{{$}}
// CHECK-6-NEXT: {{^}}@_exported public import emptyButWithLibraryEvolution{{$}}
// CHECK-6-NOT: import
