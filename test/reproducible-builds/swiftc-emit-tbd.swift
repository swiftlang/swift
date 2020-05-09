// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo %s -emit-tbd-path %t/run-1.tbd -whole-module-optimization
// RUN: %target-build-swift -O -g -module-name foo %s -emit-tbd-path %t/run-2.tbd -whole-module-optimization
// RUN: diff -u %t/run-1.tbd %t/run-2.tbd
print("foo")
