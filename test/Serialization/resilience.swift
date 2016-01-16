// RUN: rm -rf %t && mkdir %t

// This test checks that we serialize the -enable-resilience flag correctly.

// RUN: %target-swift-frontend -emit-module -o %t %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=NO-RESILIENCE %s < %t/resilience.dump.txt

// RUN: %target-swift-frontend -emit-module -o %t -enable-resilience %s
// RUN: llvm-bcanalyzer -dump %t/resilience.swiftmodule > %t/resilience2.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=RESILIENCE %s < %t/resilience2.dump.txt
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/resilience2.dump.txt

// CHECK: <MODULE_BLOCK {{.*}}>
// RESILIENCE: <IS_RESILIENT abbrevid={{[0-9]+}}/>
// NO-RESILIENCE-NOT: IS_RESILIENT
// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode

public func flip() {}
