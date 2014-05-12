// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -verify -g -o - | FileCheck %s
protocol AProtocol {
  func f() -> String;
}
class AClass : AProtocol {
  func f() -> String { return "A" }
}
class AnotherClass : AProtocol {
  func f() -> String { return "B" }
}


// CHECK-DAG: null, metadata ![[PROTOS:[0-9]+]], i32 40960, null, null, metadata !"_TtQq_F12generic_args9aFunction{{.*}}"} ; [ DW_TAG_structure_type ] [_TtQq_F12generic_args{{.*}}]
// CHECK-DAG: ![[PROTOS]] = metadata !{metadata ![[INHERIT:.*]]}
// CHECK-DAG: ![[INHERIT]] = {{.*}}metadata ![[PROTOCOL:.*]]} ; [ DW_TAG_inheritance ]
// CHECK-DAG: null, null, metadata ![[PROTOCOL]]} ; [ DW_TAG_structure_type ] [_TtMP12generic_args9AProtocol_]
// CHECK-DAG: i32 16{{.*}}, metadata ![[T:.*]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [x] [line [[@LINE+4]]]
// CHECK-DAG: ![[T]]} ; [ DW_TAG_structure_type ] [_TtQq_F12generic_args9aFunction{{.*}}]
// CHECK-DAG: i32 33{{.*}}, metadata ![[Q:.*]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [y] [line [[@LINE+2]]]
// CHECK-DAG: ![[Q]]} ; [ DW_TAG_structure_type ] [_TtQq0_F12generic_args9aFunction{{.*}}]
func aFunction<T : AProtocol, Q : AProtocol>(var x: T, var y: Q, z: String) {
     println("I am in \(z): \(x.f()) \(y.f())")
}

aFunction(AClass(),AnotherClass(),"aFunction")

struct Wrapper<T: AProtocol> {

  init<U: AProtocol>(from : Wrapper<U>) {
  // CHECK-DAG: !"_TtGV12generic_args7WrapperQq_FS0_cUS_9AProtocol__FMGS0_Q__US1___FT4fromGS0_Q___GS0_Qd____"} ; [ DW_TAG_structure_type ] [Wrapper]
    var wrapped = from
  }

  func passthrough(t: T) -> T {
    // CHECK-DAG: i32 [[@LINE+1]], metadata ![[LOCAL_T:.*]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [local]
    var local = t
    // The type of local should have the context Wrapper<T>.
    // CHECK-DAG: [[LOCAL_T]]} ; [ DW_TAG_structure_type ] [_TtQq_V12generic_args7Wrapper]
    return local
  }
}
