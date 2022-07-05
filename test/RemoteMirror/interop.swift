// REQUIRES: OS=macosx
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: remote_run

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/Inputs/interop.swift -emit-library -module-name InteropTest -o %t/%target-library-name(InteropTest)
// RUN: %target-clang %target-sanitizer-opt %S/Inputs/interop.m -framework Foundation -I %S/../../include/swift/SwiftRemoteMirror -I %S/../../include/ -o %t/InteropTest
// RUN: %target-codesign %t/%target-library-name(InteropTest)
// RUN: %target-codesign %t/InteropTest
// RUN: %target-run %t/InteropTest %t/%target-library-name(InteropTest) %platform-module-dir/%target-library-name(swiftRemoteMirror) | %FileCheck %s

// CHECK: Kind:13 Size:{{[0-9]+}} Alignment:{{[0-9]+}} Stride:{{[0-9]+}} NumFields:0
// CHECK-NEXT: Demangled name: InteropTest.C
// CHECK-NEXT: Original metadata: 0x{{[0-9A-Fa-f]+}}
// CHECK-NEXT: Looked up metadata: Metadata=0x{{[0-9A-Fa-f]+}} Library={{[0-9]+}}
// CHECK-NEXT: Kind:17 Size:{{[0-9]+}} Alignment:{{[0-9]+}} Stride:{{[0-9]+}} NumFields:26
// CHECK-NEXT:   [0]: x Offset:{{[0-9]+}} Kind:4 Type:Swift.String (Value not displayed)
// CHECK-NEXT:   [1]: y Offset:{{[0-9]+}} Kind:4 Type:Swift.Int (Value not displayed)
// CHECK-NEXT:   [2]: e Offset:{{[0-9]+}} Kind:5 Type:InteropTest.E Value: two (1)
// CHECK-NEXT:   [3]: f1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.F Value: one (0)
// CHECK-NEXT:   [4]: f2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.F Value: two (1)
// CHECK-NEXT:   [5]: f3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.F Value: three (2)
// CHECK-NEXT:   [6]: g1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.G Value: one (1)
// CHECK-NEXT:   [7]: g2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.G Value: two (0)
// CHECK-NEXT:   [8]: g3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.G Value: three (2)
// CHECK-NEXT:   [9]: g4 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.G Value: four (3)
// CHECK-NEXT:   [10]: h1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.H Value: one (1)
// CHECK-NEXT:   [11]: h2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.H Value: two (2)
// CHECK-NEXT:   [12]: h3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.H Value: three (0)
// CHECK-NEXT:   [13]: h4 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.H Value: four (3)
// CHECK-NEXT:   [14]: i1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.I Value: one (1)
// CHECK-NEXT:   [15]: i2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.I Value: two (2)
// CHECK-NEXT:   [16]: i3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.I Value: three (3)
// CHECK-NEXT:   [17]: i4 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.I Value: four (0)
// CHECK-NEXT:   [18]: j1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.J Value: one (0)
// CHECK-NEXT:   [19]: j2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.J Value: two (1)
// CHECK-NEXT:   [20]: j3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.J Value: three (2)
// CHECK-NEXT:   [21]: j4 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.J Value: four (3)
// CHECK-NEXT:   [22]: k1 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.K Value: one (1)
// CHECK-NEXT:   [23]: k2 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.K Value: two (0)
// CHECK-NEXT:   [24]: k3 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.K Value: three (2)
// CHECK-NEXT:   [25]: k4 Offset:{{[0-9]+}} Kind:6 Type:InteropTest.K Value: four (3)
