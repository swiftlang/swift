// REQUIRES: objc_interop, OS=macosx
// Temporarily disable on arm64e (rdar://88579818)
// UNSUPPORTED: CPU=arm64e

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/includes)

// Build external Swift library/module
// RUN: %target-build-swift %S/Inputs/swiftmodules/testModB.swift -parse-as-library -emit-module -emit-library -module-name testModB -o %t/includes/testModB.o

// Build external Clang library
// RUN: %target-clang %S/Inputs/cmodules/testModA.m -c -o %t/testModA.o

// Build the test into a binary
// RUN: %target-build-swift %s -parse-as-library -emit-module -emit-library -module-name ExternalConformanceCheck -I %t/includes -I %S/Inputs/cmodules -o %t/ExternalConformances %t/testModA.o %t/includes/testModB.o

// RUN: %target-swift-reflection-dump -binary-filename %t/ExternalConformances -binary-filename %platform-module-dir/%target-library-name(swiftCore) | %FileCheck %s

import testModA
import testModB

protocol myTestProto {}
extension testModBStruct : myTestProto {}
extension testModAClass : myTestProto {}

// CHECK: CONFORMANCES:
// CHECK: =============
// CHECK-DAG:  (__C.testModAClass) : ExternalConformanceCheck.myTestProto
// CHECK-DAG: _$s8testModB0aB7BStructVMn (testModB.testModBStruct) : ExternalConformanceCheck.myTestProto
