// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/clang-module-cache)

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -verify
// RUN: %validate-json %t/deps.json | %FileCheck %s

import HasBuiltinImport

// CHECK: "mainModuleName": "deps"

