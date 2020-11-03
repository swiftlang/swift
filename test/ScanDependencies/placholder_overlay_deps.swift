// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/inputs

// RUN: echo "[{" > %/t/inputs/map.json
// RUN: echo "\"moduleName\": \"Darwin\"," >> %/t/inputs/map.json
// RUN: echo "\"modulePath\": \"%/t/inputs/Darwin.swiftmodule\"," >> %/t/inputs/map.json
// RUN: echo "\"docPath\": \"%/t/inputs/Darwin.swiftdoc\"," >> %/t/inputs/map.json
// RUN: echo "\"sourceInfoPath\": \"%/t/inputs/Darwin.swiftsourceinfo\"," >> %/t/inputs/map.json
// RUN: echo "\"isFramework\": false" >> %/t/inputs/map.json
// RUN: echo "}]" >> %/t/inputs/map.json

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -placeholder-dependency-module-map-file %t/inputs/map.json -o %t/deps.json

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// REQUIRES: executable_test
// REQUIRES: objc_interop
import Metal

// Ensure the dependency on Darwin is captured even though it is a placeholder

// CHECK:   "modulePath": "Metal.swiftmodule",
// CHECK-NEXT:   "sourceFiles": [
// CHECK-NEXT:   ],
// CHECK-NEXT:   "directDependencies": [
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "CoreFoundation"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "CoreGraphics"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swiftPlaceholder": "Darwin"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "Dispatch"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "Foundation"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "IOKit"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "clang": "Metal"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "ObjectiveC"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "Swift"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       "swift": "XPC"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
