// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
import swift

class MyClass
{
    var x : Int
    init withInput (input : Int)
    {
        x = 2 * input
    }

    func do_something (input: Int) -> Int
    {
        return x * input;
    }
}

func call_me (code: () -> Void)
{
    code ()
}

func main (x: Int) -> Void
// CHECK: define void @_T9linetable4mainFT1xSi_T_
// CHECK: alloca i64, align 8
// CHECK: , !dbg ![[PROLOGUE_END:.*]]
// CHECK: ![[PROLOGUE_END]] = metadata !{i32 [[@LINE+1]], i32 1,
{
    var my_class = MyClass(withInput : 10)

    call_me (
        {
            var result = my_class.do_something(x)
            print ("Here is something you might consider doing: \(result).\n")
        }
    )

    call_me (
        {
            print ("Here is something you might consider doing: \(x).\n")
        }
    )
}

main(30)
