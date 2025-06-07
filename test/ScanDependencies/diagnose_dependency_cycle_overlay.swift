// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache

// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift &> %t/out.txt
// RUN: %FileCheck %s < %t/out.txt

// CHECK: error: module dependency cycle: 'CycleOverlay.swiftinterface -> CycleSwiftMiddle.swiftinterface -> CycleOverlay.swiftinterface'
// CHECK: note: Swift Overlay dependency of 'CycleSwiftMiddle' on 'CycleOverlay' via Clang module dependency: 'CycleSwiftMiddle.swiftinterface -> CycleClangMiddle.pcm -> CycleOverlay.pcm'

import CycleOverlay
