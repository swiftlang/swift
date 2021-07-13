// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/interop.swift -emit-library -module-name InteropTest -o %t/%target-library-name(InteropTest)
// RUN: %target-clang %S/Inputs/interop.m -framework Foundation -I %S/../../include/swift/SwiftRemoteMirror -I %S/../../include/ -o %t/InteropTest
// RUN: %target-codesign %t/%target-library-name(InteropTest)
// RUN: %target-codesign %t/InteropTest
// RUN: %target-run %t/InteropTest %t/%target-library-name(InteropTest) %platform-module-dir/%target-library-name(swiftRemoteMirror) | %FileCheck %s

// CHECK: Kind:13 Size:{{[0-9]+}} Alignment:{{[0-9]+}} Stride:{{[0-9]+}} NumFields:0
// CHECK-NEXT: Demangled name: InteropTest.C
// CHECK-NEXT: Original metadata: 0x{{[0-9A-Fa-f]+}}
// CHECK-NEXT: Looked up metadata: Metadata=0x{{[0-9A-Fa-f]+}} Library={{[0-9]+}}
// CHECK-NEXT: Kind:17 Size:{{[0-9]+}} Alignment:{{[0-9]+}} Stride:{{[0-9]+}} NumFields:3
// CHECK-NEXT:   [0]: x Offset:{{[0-9]+}} Kind:4 Type:Swift.String (Value not displayed)
// CHECK-NEXT:   [1]: y Offset:{{[0-9]+}} Kind:4 Type:Swift.Int (Value not displayed)
// CHECK-NEXT:   [2]: e Offset:{{[0-9]+}} Kind:5 Type:InteropTest.E Value: two (1)

