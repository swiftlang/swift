// Verify that Clang module PCM build commands from the dependency scanner
// include the -target flag for the Swift frontend.

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps.json | %FileCheck %s

import C

// Verify that Clang module C's command line includes -target.
// CHECK: "clang": "C"
// CHECK: "commandLine": [
// CHECK: "-emit-pcm"
// CHECK: "-target",
// CHECK-NEXT: "{{[^"]+}}"
