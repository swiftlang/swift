// RUN: %empty-directory(%t)

// RUN: %target-interop-build-clangxx -c %s -I %t -S -emit-llvm -o %t/ir.ll
// RUN: %FileCheck %s < %t/ir.ll

// UNSUPPORTED: OS=windows-msvc

unsigned char getEnumTagi8(void *p);
unsigned getEnumTagi32(void *p);

void test(void *p) {
  getEnumTagi8(p);
  getEnumTagi32(p);
}

// CHECK: declare noundef zeroext i8 @_Z12getEnumTagi8Pv(i8* noundef) #1
// CHECK: declare noundef i32 @_Z13getEnumTagi32Pv(i8* noundef) #1
