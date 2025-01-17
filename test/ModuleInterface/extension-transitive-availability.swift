// This test ensures that the parent invocation's '-application-extension' flag is inherited when building dependency modules
// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/ExtensionAvailable.swiftinterface) %S/Inputs/extension-available.swift -module-name ExtensionAvailable -I%t -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import

// RUN: %target-swift-frontend -scan-dependencies -no-scanner-module-validation %s -o %t/deps.json -I%t -application-extension -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps.json &>/dev/null
// RUN: %FileCheck %s < %t/deps.json

import ExtensionAvailable
func foo() {
    extensionAvailable()
}

// CHECK:      "directDependencies": [
// CHECK-DAG:          "swift": "ExtensionAvailable"
// CHECK-DAG:          "swift": "Swift"
// CHECK-DAG:          "swift": "SwiftOnoneSupport"

// CHECK:      "swift": "ExtensionAvailable"
// CHECK-NEXT:    },
// CHECK-NEXT:    {
// CHECK-NEXT:      "modulePath": "{{.*}}{{/|\\}}ExtensionAvailable-{{.*}}.swiftmodule",
// CHECK-NEXT:      "sourceFiles": [
// CHECK-NEXT:      ],
// CHECK-NEXT:      "directDependencies": [
// CHECK-NEXT:        {
// CHECK-NEXT:          "swift": "Swift"
// CHECK-NEXT:        },
// CHECK-NEXT:        {
// CHECK-NEXT:          "swift": "SwiftOnoneSupport"
// CHECK-NEXT:        }
// CHECK-NEXT:      ],
// CHECK-NEXT:      "linkLibraries": [
// CHECK:           "details": {
// CHECK-NEXT:        "swift": {
// CHECK-NEXT:          "moduleInterfacePath": 
// CHECK-NEXT:          "compiledModuleCandidates": [
// CHECK-NEXT:          ],
// CHECK-NEXT:          "commandLine": [
// CHECK-NEXT:            "-frontend",
// CHECK-NEXT:            "-compile-module-from-interface",
// CHECK:            "-application-extension",
