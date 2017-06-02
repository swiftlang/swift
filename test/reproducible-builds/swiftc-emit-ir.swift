// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-ir %s > %t/run-1.ir
// RUN: %target-build-swift -O -g -module-name foo -emit-ir %s > %t/run-2.ir
// RUN: diff -u %t/run-1.ir %t/run-2.ir
print("foo")
