// Verify that a Swift dependency scan propagates the parent's -sanitize=<name>
// selection into the resolved -compile-module-from-interface command for each
// swiftinterface dependency.

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/Frameworks/E.framework/Modules/E.swiftmodule
// RUN: cp %S/Inputs/Swift/E.swiftinterface %t/Frameworks/E.framework/Modules/E.swiftmodule/%module-target-triple.swiftinterface

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface %s -o %t/deps.json -F %t/Frameworks/ -sdk %t -sanitize=address

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json E > %t/E.cmd
// RUN: %FileCheck %s -input-file=%t/E.cmd

import E

// CHECK: "-sanitize=address"
