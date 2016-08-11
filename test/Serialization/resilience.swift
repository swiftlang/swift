// RUN: rm -rf %t && mkdir %t

// This test checks that we serialize the -enable-resilience and -sil-serialize-all
// flags correctly.

// RUN: %target-swift-frontend -emit-module -o %t %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=DEFAULT %s < %t/resilience.dump.txt

// RUN: %target-swift-frontend -emit-module -o %t -enable-resilience %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience2.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=RESILIENCE %s < %t/resilience2.dump.txt

// RUN: %target-swift-frontend -emit-module -o %t -sil-serialize-all %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience3.dump.txt
// RUN: %FileCheck -check-prefix=CHECK -check-prefix=FRAGILE %s < %t/resilience3.dump.txt

// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/resilience2.dump.txt

// CHECK: <MODULE_BLOCK {{.*}}>
// RESILIENCE: <RESILIENCE_STRATEGY abbrevid={{[0-9]+}} op0=1/>
// FRAGILE: <RESILIENCE_STRATEGY abbrevid={{[0-9]+}} op0=2/>
// DEFAULT-NOT: RESILIENCE_STRATEGY

// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode

public func flip() {}
