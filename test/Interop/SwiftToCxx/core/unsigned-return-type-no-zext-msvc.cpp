// RUN: %empty-directory(%t)

// RUN: %target-interop-build-clangxx -c %s -I %t -S -emit-llvm -o %t/ir.ll
// RUN: %FileCheck %s < %t/ir.ll

// REQUIRES: OS=windows-msvc

unsigned char getEnumTagi8(void *p);
unsigned getEnumTagi32(void *p);

void test(void *p) {
  getEnumTagi8(p);
  getEnumTagi32(p);
}

// CHECK: declare dso_local noundef i8 @"?getEnumTagi8@@YAEPEAX@Z"(ptr noundef) #1
// CHECK: declare dso_local noundef i32 @"?getEnumTagi32@@YAIPEAX@Z"(ptr noundef) #1
