// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t.module-cache %s -o %t.deps.json -I %S/Inputs/CHeaders -verify

// RUN: %validate-json %t.deps.json | %FileCheck %s
// CHECK: "clang": "X_Private"
// CHECK: "clang": "Y"
// CHECK-NOT: "clang": "Y_Private"
import X.Private
import Y.Private
