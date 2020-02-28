// Make sure we don't consider submodules, because there are...a lot of them.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -enable-cross-import-overlays -Xllvm -debug-only=swift-name-binding %s 2>%t.txt
// RUN: %FileCheck %s < %t.txt
// RUN: %FileCheck --check-prefix=NEGATIVE %s < %t.txt

// Note: It is a bit confusing but the -debug-only flag means that this info
// will be printed only in the presence of assertions.
//
// REQUIRES: asserts

import Darwin
import ctypes

// CHECK-DAG: Discovering cross-imports for 'ctypes' -> 'Darwin'
// CHECK-DAG: Discovering cross-imports for 'Darwin' -> 'ctypes'

// NEGATIVE-NOT: Discovering cross-imports for 'bits' -> '{{.*}}'
// NEGATIVE-NOT: Discovering cross-imports for '{{.*}}' -> 'bits'
// NEGATIVE-NOT: Discovering cross-imports for 'MacTypes' -> '{{.*}}'
// NEGATIVE-NOT: Discovering cross-imports for '{{.*}}' -> 'MacTypes'
