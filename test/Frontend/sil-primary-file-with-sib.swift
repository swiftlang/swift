// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-sib %s -module-name test -o %t/test.sib
// RUN: %target-build-swift -Xfrontend -disable-llvm-optzns -emit-ir %s -module-name test -o %t/test-orig.ll
// RUN: %target-swift-frontend -emit-ir -primary-file %S/Inputs/sil-primary-file-with-sib.sil %t/test.sib -module-name test -o %t/test-func.ll
// RUN: %llvm-link %t/test-orig.ll -override %t/test-func.ll -o %t/test.bc
// RUN: %target-swift-frontend -c %t/test.bc -o %t/test.o
// RUN: %target-build-swift %t/test.o -o %t/test
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test | %FileCheck %s
// REQUIRES: executable_test


// CHECK: The number: 1504

@inline(never)
func return_a_number() -> Int64 {
  return 1995
}

print("The number: \(return_a_number())")
