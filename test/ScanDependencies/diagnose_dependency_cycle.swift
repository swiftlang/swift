// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache

// RUN: not %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 &> %t/out.txt

// RUN: %FileCheck %s < %t/out.txt

// CHECK: module dependency cycle: 'CycleOne.swiftinterface -> CycleTwo.swiftinterface -> CycleThree.swiftinterface -> CycleOne.swiftinterface'

import CycleOne
