// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t.foo.swiftmodule -module-cache-path %t.module-cache -I %S/Inputs/CHeaders -I %S/Inputs/Swift %s -target x86_64-apple-macosx10.14

// Without -clang-target, we build two X.pcm
// RUN: find %t.module-cache -name "X-*.pcm" | count 2

// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t.foo.swiftmodule -module-cache-path %t.module-cache -I %S/Inputs/CHeaders -I %S/Inputs/Swift %s -target x86_64-apple-macosx10.14 -clang-target x86_64-apple-macosx10.14

// With -clang-target, we build one X.pcm
// RUN: find %t.module-cache -name "X-*.pcm" | count 1

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t.module-cache %s -o %t.deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -target x86_64-apple-macosx10.14 -clang-target x86_64-apple-macosx10.14
// RUN: %FileCheck %s < %t.deps.json

// CHECK: "-clang-target"
// CHECK-NEXT: "x86_64-apple-macosx10.14"

import X
import XWithTarget
