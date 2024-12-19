// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/Frameworks
// RUN: mkdir -p %t/Frameworks/E.framework/
// RUN: mkdir -p %t/Frameworks/E.framework/Modules
// RUN: mkdir -p %t/Frameworks/E.framework/Modules/E.swiftmodule

// Copy over the interface
// RUN: cp %S/Inputs/Swift/E.swiftinterface %t/Frameworks/E.framework/Modules/E.swiftmodule/%module-target-triple.swiftinterface

// Run the scan
// RUN: %target-swift-frontend -scan-dependencies -enable-ossa-modules -disable-implicit-swift-modules -module-load-mode prefer-interface %s -o %t/deps.json -F %t/Frameworks/ -sdk %t
// RUN: %validate-json %t/deps.json | %FileCheck %s

import E

// CHECK: E.swiftmodule/{{.*}}.swiftinterface
// CHECK: "commandLine": [
// CHECK: "-enable-ossa-modules"
// CHECK: ]
