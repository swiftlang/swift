// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx -c %s -I %t -S -emit-llvm -o %t/ir.ll
// RUN: %FileCheck %s < %t/ir.ll

unsigned char getEnumTagi8(void *p);
unsigned getEnumTagi32(void *p);

void test(void *p) {
  getEnumTagi8(p);
  getEnumTagi32(p);
}

// CHECK: declare zeroext i8 @_Z12getEnumTagi8Pv(i8*) #1
// CHECK: declare i32 @_Z13getEnumTagi32Pv(i8*) #1
