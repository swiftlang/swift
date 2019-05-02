// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t/empty.swiftinterface %s -swift-version 4 2>&1 | %FileCheck -DVERSION=4 %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t/empty.swiftinterface %s -swift-version 4.2 2>&1 | %FileCheck -DVERSION=4.2 %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t/empty.swiftinterface %s -swift-version 5 2>&1 | %FileCheck -check-prefix=NEGATIVE -allow-empty %s
// RUN: ls %t/empty.swiftinterface

// CHECK: warning: module interfaces are only supported with Swift language version 5 or later (currently using -swift-version [[VERSION]])
// NEGATIVE-NOT: warning:
// NEGATIVE-NOT: error:
