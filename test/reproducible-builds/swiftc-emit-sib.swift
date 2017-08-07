// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-sib %s -o %t/run-1.sib
// RUN: %target-build-swift -O -g -module-name foo -emit-sib %s -o %t/run-2.sib
// RUN: cmp %t/run-1.sib %t/run-2.sib
print("foo")
