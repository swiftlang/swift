// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
// CHECK: @closure0
// CHECK: , !dbg ![[DBG:.*]]

func get_truth (input : Int) -> Int
{
    return input % 2
}

func call_me (input : Int) -> Void
{
// rdar://problem/14627460
// A closure should have a line number in the debug info and a scope line of 0.
// CHECK-DAG: "closure0"{{.*}}[ DW_TAG_subprogram ] [line [[@LINE+3]]] [def] [scope 0]
// But not in the line table.
// CHECK-DAG: ![[DBG]] = metadata !{i32 0, i32 0,
    if input != 0 && ( get_truth (input * 2 + 1) > 0 )
    {
        println ("Whew, passed that test.")
    }

}

call_me(5)
