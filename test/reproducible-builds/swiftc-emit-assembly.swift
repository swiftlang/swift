// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-assembly %s > %t/run-1.S
// RUN: %target-build-swift -O -g -module-name foo -emit-assembly %s > %t/run-2.S
// RUN: diff -u %t/run-1.S %t/run-2.S
print("foo")
