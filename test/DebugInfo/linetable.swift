// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
// RUN: %swift -target x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s --check-prefix ASM-CHECK
import Swift

class MyClass
{
    var x : Int
    init(input: Int)
    {
        x = 2 * input
    }

    func do_something(input: Int) -> Int
    {
        return x * input;
    }
}

func call_me(code: () -> Void)
{
    code ()
}

func main(x: Int) -> Void
// CHECK: define void @_TF9linetable4main
{
    var my_class = MyClass(input: 10)
// Linetable continuity. Don't go into the closure expression.
// ASM-CHECK: .loc	1 [[@LINE+1]] 5
    call_me (
// ASM-CHECK-NOT: .loc	1 [[@LINE+1]] 5
// CHECK: @_TFF9linetable4mainFSiT_U_FT_T__promote0
        {
            var result = my_class.do_something(x)
            print ("Here is something you might consider doing: \(result).\n")
// CHECK: call {{.*}} @swift_release {{.*}}, !dbg ![[CLOSURE_END:.*]]
// CHECK-NEXT: ret void, !dbg ![[CLOSURE_END]]
// CHECK: ![[CLOSURE_END]] = metadata !{i32 [[@LINE+1]]
        }
    )

// ASM-CHECK: .loc	1 [[@LINE+1]] 5
    call_me (
        {
            print ("Here is something you might consider doing: \(x).\n")
        }
    )

// The swift_releases at the end should not jump to the point where
// that memory was retained/allocated and also not to line 0.
// ASM-CHECK-NOT: .loc	1 0 0
// ASM-CHECK: .loc	1 [[@LINE+2]] 1
// ASM-CHECK: ret
}

// ASM-CHECK:__TFF9linetable4main{{.*}}_promote0:
// ASM-CHECK-NOT: retq
// The end-of-prologue should have a valid location.
// ASM-CHECK: .loc	1 34 {{[0-9]+}} prologue_end


main(30)
