// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-module %s -o %t/run-1.module
// RUN: %target-build-swift -O -g -module-name foo -emit-module %s -o %t/run-2.module
// RUN: cmp %t/run-1.module %t/run-2.module
// RUN: cmp %t/run-1.swiftdoc %t/run-2.swiftdoc
print("foo")
