// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/includes)
// RUN: echo "[MyProto]" > %t/protocols.json

// Build external Swift library/module to also check conformances to external protocols
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 %S/../Reflection/Inputs/swiftmodules/testModB.swift -parse-as-library -emit-module -emit-library -module-name testModB -o %t/includes/testModB.o

// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -typecheck -emit-const-values-path %t/ExtractExternalConformances.swiftconstvalues -const-gather-protocols-file %t/protocols.json -primary-file %s -I %t/includes
// RUN: cat %t/ExtractExternalConformances.swiftconstvalues 2>&1 | %FileCheck %s
import testModB

public protocol MyProto { }

extension TestExternalConformanceStruct: MyProto {}

extension TestExternalConformanceStruct: TestExternalConformanceProtocol {}

// CHECK: [
// CHECK-NEXT:   {
// CHECK-NEXT:     "typeName": "testModB.TestExternalConformanceStruct",
// CHECK-NEXT:     "mangledTypeName": "8testModB29TestExternalConformanceStructV",
// CHECK-NEXT:     "kind": "struct",
// CHECK-NEXT:     "conformances": [
// CHECK-NEXT:       "testModB.testModBProtocol",
// CHECK-NEXT:       "ExtractExternalConformances.MyProto",
// CHECK-NEXT:       "testModB.TestExternalConformanceProtocol"
// CHECK-NEXT:     ],
// CHECK-NEXT:     "allConformances": [
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "testModB.testModBProtocol",
// CHECK-NEXT:         "conformanceDefiningModule": "testModB"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "ExtractExternalConformances.MyProto",
// CHECK-NEXT:         "conformanceDefiningModule": "ExtractExternalConformances"
// CHECK-NEXT:       },
// CHECK-NEXT:       {
// CHECK-NEXT:         "protocolName": "testModB.TestExternalConformanceProtocol",
// CHECK-NEXT:         "conformanceDefiningModule": "ExtractExternalConformances"
// CHECK-NEXT:       }
// CHECK-NEXT:     ],
// CHECK-NEXT:     "associatedTypeAliases": [],
// CHECK-NEXT:     "properties": []
// CHECK-NEXT:   }
// CHECK-NEXT: ]
