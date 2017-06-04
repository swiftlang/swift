// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -g -module-name foo -emit-sibgen %s -o %t/run-1.sibgen
// RUN: %target-build-swift -O -g -module-name foo -emit-sibgen %s -o %t/run-2.sibgen
// RUN: cmp %t/run-1.sibgen %t/run-2.sibgen
print("foo")
