// RUN: %swift %clang-importer-sdk -target x86_64-apple-macosx10.9 %s -emit-ir -o - | FileCheck %s

import cfuncs

// CHECK: call void @cfunc1
cfunc1()

// CHECK: call double @pow(double 3.141590e+00, double 2.718280e+00)
var d = pow(3.14159, 2.71828)

// CHECK: call i32 @"\01_something_else"(i32 17)
renamed(17)

// CHECK: call void @exit(i32 17)
exit(17)

