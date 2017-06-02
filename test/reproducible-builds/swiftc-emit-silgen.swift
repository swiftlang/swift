// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-silgen %s > %t/run-1.silgen
// RUN: %target-build-swift -O -g -module-name foo -emit-silgen %s > %t/run-2.silgen
// RUN: diff -u %t/run-1.silgen %t/run-2.silgen
print("foo")
