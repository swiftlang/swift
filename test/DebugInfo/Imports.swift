// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
// CHECK: i32 [[@LINE+1]], metadata !"basic"} ; [ DW_TAG_imported_module ]
import basic
import POSIX

println(basic.foo(1, 2))
