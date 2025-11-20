// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
// RUN: %validate-json %t/deps.json | %FileCheck %s

// CHECK: "mainModuleName": "deps"
// CHECK: "swift": "deps"
// CHECK: "modulePath": "deps.swiftmodule"

// CHECK: "imports": [
// CHECK-NEXT:        {
// CHECK-NEXT:          "identifier": "Swift"
// CHECK-NEXT:          "accessLevel": "public"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "identifier": "SwiftOnoneSupport"
// CHECK-NEXT:          "accessLevel": "public"
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "identifier": "C"
// CHECK-NEXT:          "accessLevel": "internal"
// CHECK-NEXT:          "importLocations": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "bufferIdentifier": "{{.*}}import_infos.swift"
// CHECK-NEXT:              "linuNumber": 65
// CHECK-NEXT:              "columnNumber": 17
// CHECK-NEXT:            }
// CHECK-NEXT:            {
// CHECK-NEXT:              "bufferIdentifier": "{{.*}}import_infos.swift"
// CHECK-NEXT:              "linuNumber": 72
// CHECK-NEXT:              "columnNumber": 16
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "identifier": "E"
// CHECK-NEXT:          "accessLevel": "public"
// CHECK-NEXT:          "importLocations": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "bufferIdentifier": "{{.*}}import_infos.swift"
// CHECK-NEXT:              "linuNumber": 67
// CHECK-NEXT:              "columnNumber": 15
// CHECK-NEXT:            }
// CHECK-NEXT:            {
// CHECK-NEXT:              "bufferIdentifier": "{{.*}}import_infos.swift"
// CHECK-NEXT:              "linuNumber": 68
// CHECK-NEXT:              "columnNumber": 8
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:        {
// CHECK-NEXT:          "identifier": "G"
// CHECK-NEXT:          "accessLevel": "fileprivate"
// CHECK-NEXT:          "importLocations": [
// CHECK-NEXT:            {
// CHECK-NEXT:              "bufferIdentifier": "{{.*}}import_infos.swift"
// CHECK-NEXT:              "linuNumber": 70
// CHECK-NEXT:              "columnNumber": 20
// CHECK-NEXT:            }
// CHECK-NEXT:          ]
// CHECK-NEXT:        }
// CHECK-NEXT:      ]

internal import C

public import E
import E

fileprivate import G

private import C
