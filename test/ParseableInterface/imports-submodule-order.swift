// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s -I %S/Inputs/imports-submodule-order/ | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path - %s -I %S/Inputs/imports-submodule-order/ -D XY | %FileCheck %s

#if XY
@_exported import X.Submodule
@_exported import Y.Submodule

#else
@_exported import Y.Submodule
@_exported import X.Submodule

#endif

// The order below is not alphabetical, just deterministic given a set of
// imports across any number of files and in any order.

// CHECK-NOT: import
// CHECK: import X.Submodule{{$}}
// CHECK-NEXT: import Y.Submodule{{$}}
// CHECK-NEXT: {{^}}import Swift{{$}}
// CHECK-NEXT: import X{{$}}
// CHECK-NEXT: import Y{{$}}
// CHECK-NOT: import
