// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// CHECK-DAG: ![[TY0:[0-9]+]] = {{.*}} [ DW_TAG_structure_type ] [{{.*}}Ty0{{.*}}] [line [[@LINE+1]]
class [objc] Ty0 {}


// CHECK-DAG: i32 [[@LINE+1]], metadata ![[TY0]],{{.*}}[ DW_TAG_variable ] [{{.*}}strong{{.*}}]
var strong : Ty0
