// RUN: echo '#include "print_clang_header_swift_name.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print \
// RUN:     %S/Inputs/print_clang_header_swift_name.h -skip-unavailable --cc-args %target-cc-options \
// RUN:     -isysroot %clang-importer-sdk-path -fsyntax-only %t.m -I %S/Inputs | %FileCheck %s

// REQUIRES: objc_interop

// CHECK: enum Normal : Int {
// CHECK-NOT: {{^}}}
// CHECK: case one
// CHECK-NEXT: case two
// CHECK-NEXT: case three
// CHECK-NEXT: }

// CHECK: enum SwiftEnum : Int {
// CHECK-NOT: {{^}}}
// CHECK: case one
// CHECK-NEXT: case two
// CHECK-NEXT: case three
// CHECK-NEXT: }

// CHECK: enum SwiftEnumTwo : Int {
// CHECK-NOT: {{^}}}
// CHECK: case SwiftEnumTwoA
// CHECK-NEXT: case SwiftEnumTwoB
// CHECK-NEXT: case SwiftEnumTwoC
// CHECK-NEXT: }
