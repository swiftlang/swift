// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-tbd %s -o %t/run-1.tbd
// RUN: %target-build-swift -O -g -module-name foo -emit-tbd %s -o %t/run-2.tbd
// RUN: diff -u %t/run-1.tbd %t/run-2.tbd
print("foo")
