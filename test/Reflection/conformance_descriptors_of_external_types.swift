// REQUIRES: objc_interop, OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/includes)

// Build external Swift library/module
// RUN: %target-build-swift -target %target-swift-5.2-abi-triple  %S/Inputs/swiftmodules/testModB.swift -parse-as-library -emit-module -emit-library -module-name testModB -o %t/includes/testModB.o

// Build external Clang library
// RUN: %target-clang %S/Inputs/cmodules/testModA.m -c -o %t/testModA.o

// Build the test into a binary
// RUN: %target-build-swift -target %target-swift-5.2-abi-triple   %s -parse-as-library -emit-module -emit-library -module-name ExternalConformanceCheck -I %t/includes -I %S/Inputs/cmodules -o %t/ExternalConformances %t/testModA.o %t/includes/testModB.o

// RUN: %target-swift-reflection-dump %t/ExternalConformances %platform-module-dir/%target-library-name(swiftCore) | %FileCheck %s

import testModA
import testModB

protocol myTestProto {}
extension testModBStruct : myTestProto {}
extension testModAClass : myTestProto {}

// CHECK: CONFORMANCES:
// CHECK: =============
// CHECK-DAG:  (__C.testModAClass) : ExternalConformanceCheck.myTestProto
// CHECK-DAG: 8testModB0aB7BStructV (testModB.testModBStruct) : ExternalConformanceCheck.myTestProto
