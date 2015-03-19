// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t %s
// RUN: llvm-bcanalyzer -dump %t/testability.swiftmodule > %t/testability.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=NO-TESTING %s < %t/testability.dump.txt
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t -enable-testing %s
// RUN: llvm-bcanalyzer -dump %t/testability.swiftmodule > %t/testability2.dump.txt
// RUN: FileCheck -check-prefix=CHECK -check-prefix=TESTING %s < %t/testability2.dump.txt
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/testability2.dump.txt

// CHECK: <MODULE_BLOCK {{.*}}>
// TESTING: <IS_TESTABLE abbrevid={{[0-9]+}}/>
// NO-TESTING-NOT: IS_TESTABLE
// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode
