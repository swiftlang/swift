// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -O -g -module-name foo -emit-sil %s -o %t/run-1.sil
// RUN: %target-build-swift -O -g -module-name foo -emit-sil %s -o %t/run-2.sil
// RUN: diff -u %t/run-1.sil %t/run-2.sil
print("foo")
