// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/empty.swiftinterface %s -swift-version 4 2>&1 | %FileCheck -DVERSION=4 %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/empty.swiftinterface %s -swift-version 4.2 2>&1 | %FileCheck -DVERSION=4.2 %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/empty.swiftinterface %s -swift-version 4.2 -enable-library-evolution 2>&1 | %FileCheck -check-prefix=CHECK-VERSION-ONLY -DVERSION=4.2 %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/empty.swiftinterface %s -swift-version 5 2>&1 | %FileCheck -check-prefix=CHECK-EVOLUTION-ONLY %s
// RUN: ls %t/empty.swiftinterface

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/empty.swiftinterface %s -swift-version 5 -enable-library-evolution 2>&1 | %FileCheck -check-prefix=NEGATIVE -allow-empty %s
// RUN: ls %t/empty.swiftinterface

// CHECK-DAG: warning: module interfaces are only supported with Swift language version 5 or later (currently using -swift-version [[VERSION]])
// CHECK-DAG: warning: module interfaces are only supported with -enable-library-evolution

// CHECK-VERSION-ONLY-NOT: warning:
// CHECK-VERSION-ONLY: warning: module interfaces are only supported with Swift language version 5 or later (currently using -swift-version [[VERSION]])
// CHECK-VERSION-ONLY-NOT: warning:

// CHECK-EVOLUTION-ONLY-NOT: warning:
// CHECK-EVOLUTION-ONLY: warning: module interfaces are only supported with -enable-library-evolution
// CHECK-EVOLUTION-ONLY-NOT: warning:

// NEGATIVE-NOT: warning:
// NEGATIVE-NOT: error:
