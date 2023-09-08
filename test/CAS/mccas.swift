// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-backend-mode=verify -cas-path %t/cas -o %t/test-verify.o
// RUN: %llvm-dwarfdump %t/test-verify.o | %FileCheck %s --check-prefix=VERIFY-FILE
// VERIFY-FILE: .debug_info

// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-backend-mode=native -cas-path %t/cas -o %t/test-native.o
// RUN: %llvm-dwarfdump %t/test-native.o | %FileCheck %s --check-prefix=NATIVE-FILE
// NATIVE-FILE: .debug_info

// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-backend-mode=casid -cas-path %t/cas -o %t/test-casid.id
// RUN: cat %t/test-casid.id | %FileCheck %s --check-prefix=CASID-FILE
// CASID-FILE: CASID:Jllvmcas://{{.*}}

// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-emit-casid-file -cas-backend-mode=verify -cas-path %t/cas -o %t/test-verify-emit.o
// RUN: cat %t/test-verify-emit.o.casid | %FileCheck %s --check-prefix=VERIFY-EMIT
// VERIFY-EMIT: CASID:Jllvmcas://{{.*}}

// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-emit-casid-file -cas-backend-mode=native -cas-path %t/cas -o %t/test-native-emit.o
// RUN: cat %t/test-native-emit.o.casid | %FileCheck %s --check-prefix=NATIVE-EMIT
// NATIVE-EMIT: CASID:Jllvmcas://{{.*}}

// RUN: %target-swift-frontend -c %s -target arm64-apple-darwin23.0.0 -g -cas-backend -cas-emit-casid-file -cas-backend-mode=casid -cas-path %t/cas -o %t/test.id
// RUN: not cat %t/test.id.casid

// REQUIRES: _234234

func testFunc() {}
