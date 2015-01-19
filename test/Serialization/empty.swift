// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-stdlib -o %t %s
// RUN: llvm-bcanalyzer -dump %t/empty.swiftmodule > %t/empty.dump.txt
// RUN: FileCheck %s < %t/empty.dump.txt
// RUN: FileCheck -check-prefix=NEGATIVE %s < %t/empty.dump.txt

// CHECK: <MODULE_BLOCK {{.*}}>
// CHECK: <MODULE_NAME abbrevid={{[0-9]+}}/> blob data = 'empty'
// CHECK: </MODULE_BLOCK>
// CHECK-NOT: <MODULE_BLOCK {{.*}}>

// NEGATIVE-NOT: UnknownCode
