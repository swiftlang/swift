// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -module-name CycleOverlay &> %t/out.txt

// RUN: %FileCheck %s < %t/out.txt

// CHECK: module dependency cycle: 'CycleOverlay (Source Target) -> CycleSwiftMiddle.swiftinterface -> CycleOverlay.swiftinterface'
// CHECK: Swift Overlay dependency of 'CycleSwiftMiddle' on 'CycleOverlay' via Clang module dependency: 'CycleSwiftMiddle.swiftinterface -> CycleClangMiddle.pcm -> CycleOverlay.pcm'

import CycleSwiftMiddle


