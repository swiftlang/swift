// RUN: %empty-directory(%t)

// This test checks that we serialize the -enable-library-evolution
// flag.

// RUN: %target-swift-frontend -emit-module -o %t %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=DEFAULT %s < %t/resilience.dump.txt

// RUN: %target-swift-frontend -emit-module -o %t -enable-library-evolution %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience2.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=RESILIENCE %s < %t/resilience2.dump.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/resilience2.dump.txt

// FIXME: The alternate -enable-resilience flag is going away soon.

// RUN: %target-swift-frontend -emit-module -o %t -enable-resilience %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience2.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=RESILIENCE %s < %t/resilience2.dump.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/resilience2.dump.txt

// CHECK: <MODULE_BLOCK {{.*}}>
// RESILIENCE: <RESILIENCE_STRATEGY abbrevid={{[0-9]+}} op0=1/>
// FRAGILE-NOT: <RESILIENCE_STRATEGY abbrevid={{[0-9]+}}
// DEFAULT-NOT: RESILIENCE_STRATEGY

// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode

public func flip() {}
