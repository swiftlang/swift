// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/Hello.swiftinterface -module-name Hello -module-abi-name Goodbye %s
// RUN: %FileCheck %s < %t/Hello.swiftinterface

// CHECK: -module-abi-name Goodbye
