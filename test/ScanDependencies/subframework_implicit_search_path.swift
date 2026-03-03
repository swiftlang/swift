// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/mock.sdk/System/Library)

// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks)
// RUN: %empty-directory(%t/mock.sdk/System/Library/SubFrameworks)

// RUN: %empty-directory(%t/mock.sdk/System/Library/Frameworks/E.framework/Modules/E.swiftmodule)
// RUN: %empty-directory(%t/mock.sdk/System/Library/SubFrameworks/SubE.framework/Modules/SubE.swiftmodule)

// RUN: cp %S/Inputs/Swift/E.swiftinterface %t/mock.sdk/System/Library/Frameworks/E.framework/Modules/E.swiftmodule/%target-swiftinterface-name
// RUN: cp %S/Inputs/Swift/SubE.swiftinterface %t/mock.sdk/System/Library/SubFrameworks/SubE.framework/Modules/SubE.swiftmodule/%target-swiftinterface-name

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -sdk %t/mock.sdk
// RUN: %validate-json %t/deps.json | %FileCheck %s

import E
import SubE

// CHECK: "mainModuleName": "deps"
// CHECK: "swift": "E"
// CHECK: "swift": "SubE"
