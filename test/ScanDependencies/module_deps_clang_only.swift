// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-clang-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 -disable-implicit-swift-modules -module-name C

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// CHECK:  	    "mainModuleName": "C",
// CHECK-NEXT:  "modules": [
// CHECK-NEXT:  {
// CHECK-NEXT:    "clang": "C"
// CHECK-NEXT:  },
// CHECK-NEXT:  {
// CHECK-NEXT:    "modulePath": "C.pcm",
// CHECK-NEXT:    "sourceFiles": [


// CHECK:       "directDependencies": [
// CHECK-NEXT:  {
// CHECK-NEXT:    "clang": "B"
// CHECK-NEXT:  }
