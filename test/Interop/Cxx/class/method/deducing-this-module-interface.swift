// RUN: %target-swift-ide-test -print-module -module-to-print=DeducingThis -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++23 | %FileCheck %s

// This mostly ensures that we don't import deducing this method with an incorrect signature.

// CHECK: struct HasDeducingThis {
// CHECK-NOT:   deducingRef
