/// Swift 7 variant to imports.swift.

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -emit-module -o %t/nonResilient.swiftmodule %t/empty.swift
// RUN: %target-swift-frontend -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -emit-module -o %t/resilient.swiftmodule %t/empty.swift -enable-library-evolution

/// Check errors.
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %t/clientWithError.swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -verify -enable-upcoming-feature InternalImportsByDefault

/// Check Swift 7 imports printed in swiftinterface from 2 source files.
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %t/main.swift %t/main-other.swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %S/Inputs/imports-clang-modules/ -I %t -verify -enable-upcoming-feature InternalImportsByDefault
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -I %S/Inputs/imports-clang-modules/ -I %t
// RUN: %FileCheck -implicit-check-not BAD -check-prefix CHECK-7 %s < %t.swiftinterface

// REQUIRES: swift_feature_InternalImportsByDefault

//--- empty.swift

//--- main.swift
@_exported public import resilient
public import B.B2 // expected-warning {{public import of 'B' was not used in public declarations or inlinable code}}

public import func C.c // expected-warning {{public import of 'C' was not used in public declarations or inlinable code}}
// expected-warning @-1 {{scoped imports are not yet supported in module interfaces}}
import D
@_implementationOnly import Secret_BAD

@_implementationOnly import NotSoSecret // expected-note {{imported as implementation-only here}}
public import NotSoSecret2 // expected-warning {{'NotSoSecret2' inconsistently imported as implementation-only}}
// expected-warning @-1 {{public import of 'NotSoSecret2' was not used in public declarations or inlinable code}}

//--- main-other.swift
public import A // expected-warning {{public import of 'A' was not used in public declarations or inlinable code}}
public import B.B3 // expected-warning {{public import of 'B' was not used in public declarations or inlinable code}}
public import D // expected-warning {{public import of 'D' was not used in public declarations or inlinable code}}

public import NotSoSecret // expected-warning {{'NotSoSecret' inconsistently imported as implementation-only}}
// expected-warning @-1 {{public import of 'NotSoSecret' was not used in public declarations or inlinable code}}
@_implementationOnly import NotSoSecret2 // expected-note {{imported as implementation-only here}}
//--- clientWithError.swift
@_exported public import nonResilient // expected-error {{module 'nonResilient' was not compiled with library evolution support; using it means binary compatibility for 'clientWithError' can't be guaranteed}}

// CHECK-7-NOT: import
// CHECK-7: {{^}}public import A{{$}}
// CHECK-7-NEXT: {{^}}public import B{{$}}
// CHECK-7-NEXT: {{^}}public import B.B2{{$}}
// CHECK-7-NEXT: {{^}}public import B.B3{{$}}
// CHECK-7-NEXT: {{^}}public import C/*.c*/{{$}}
// CHECK-7-NEXT: {{^}}public import D{{$}}
// CHECK-7-NEXT: {{^}}public import NotSoSecret{{$}}
// CHECK-7-NEXT: {{^}}public import NotSoSecret2{{$}}
// CHECK-7-NEXT: {{^}}public import Swift{{$}}
// CHECK-7-NEXT: {{^}}@_exported public import resilient{{$}}
// CHECK-7-NOT: import
