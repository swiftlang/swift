// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo %s -emit-tbd-path %t/run-1.tbd -force-single-frontend-invocation
// RUN: %target-build-swift -O -g -module-name foo %s -emit-tbd-path %t/run-2.tbd -force-single-frontend-invocation
// RUN: diff -u %t/run-1.tbd %t/run-2.tbd
print("foo")
