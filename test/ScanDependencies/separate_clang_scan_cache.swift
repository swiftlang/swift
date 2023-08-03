// RUN: %empty-directory(%t.module-cache)
// RUN: %empty-directory(%t.scanner-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t.module-cache -clang-scanner-module-cache-path %t.scanner-cache %s -o %t.deps.json -I %S/Inputs/SwiftDifferent -dump-clang-diagnostics 2>&1 | %FileCheck %s --check-prefix=CHECK-CLANG-COMMAND
// RUN: %validate-json %t.deps.json | %FileCheck %s --check-prefix=CHECK-DEPS

// CHECK-CLANG-COMMAND: '-fmodules-cache-path={{.*}}.scanner-cache'

// CHECK-DEPS: "swift": "A"
// CHECK-DEPS:            "swift": "A"
// CHECK-DEPS-NEXT:    },
// CHECK-DEPS-NEXT:    {
// CHECK-DEPS-NEXT:        "modulePath": "{{.*}}.module-cache/A-{{.*}}.swiftmodule",
// CHECK-DEPS-NEXT:      "sourceFiles": [
import A
