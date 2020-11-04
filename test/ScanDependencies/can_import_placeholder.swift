// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"SomeExternalModule\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/SomeExternalModule.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"docPath\": \"%/t/inputs/SomeExternalModule.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"sourceInfoPath\": \"%/t/inputs/SomeExternalModule.swiftsourceinfo\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -placeholder-dependency-module-map-file %t/inputs/map.json -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// REQUIRES: executable_test
// REQUIRES: objc_interop
#if canImport(SomeExternalModule)
import SomeExternalModule
#endif

// CHECK: "mainModuleName": "deps"

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: sourceFiles
// CHECK-NEXT: can_import_placeholder.swift

// CHECK: directDependencies
// CHECK-NEXT: {
// CHECK-NEXT: "swiftPlaceholder": "SomeExternalModule"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "Swift"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "SwiftOnoneSupport"
// CHECK-NEXT: }
// CHECK-NEXT: {
// CHECK-NEXT: "swift": "F"
// CHECK-NEXT: }
// CHECK-NEXT: ],

