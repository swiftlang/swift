// RUN: %empty-directory(%t)
// RUN: env SWIFTC_MAXIMUM_DETERMINISM=1 %target-swiftc_driver -j 4 -num-threads 10 -c -o %t/out.o %s >%t/out.txt 2>&1
// RUN: %FileCheck -input-file %t/out.txt %s
// CHECK: remark: SWIFTC_MAXIMUM_DETERMINISM overriding -j
// CHECK: remark: SWIFTC_MAXIMUM_DETERMINISM overriding -num-threads
print(1)
