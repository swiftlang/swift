// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
// RUN: %target-swift-frontend %s -S -g -o - | FileCheck %s --check-prefix ASM-CHECK

// REQUIRES: CPU=i386_or_x86_64

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
// CHECK: define hidden void @_TF9linetable4main
{
    var my_class = MyClass(input: 10)
// Linetable continuity. Don't go into the closure expression.
// ASM-CHECK: .loc	[[FILEID:[0-9]]] [[@LINE+1]] 5
    call_me (
// ASM-CHECK-NOT: .loc	[[FILEID]] [[@LINE+1]] 5
// CHECK: @_TTSf2d_i_n___TFF9linetable4mainFSiT_U_FT_T_
        {
            var result = my_class.do_something(x)
            print ("Here is something you might consider doing: \(result).\n")
// CHECK: call {{.*}} @swift_release {{.*}}
// CHECK: call {{.*}} @swift_release {{.*}}, !dbg ![[CLOSURE_END:.*]]
// CHECK-NEXT: ret void, !dbg ![[CLOSURE_END]]
// CHECK: ![[CLOSURE_END]] = !DILocation(line: [[@LINE+1]],
        }
    )

// ASM-CHECK: .loc	[[FILEID]] [[@LINE+1]] 5
    call_me (
        {
            print ("Here is something you might consider doing: \(x).\n")
        }
    )

// The swift_releases at the end should not jump to the point where
// that memory was retained/allocated and also not to line 0.
// ASM-CHECK-NOT: .loc	[[FILEID]] 0 0
// ASM-CHECK: .loc	[[FILEID]] [[@LINE+2]] 1
// ASM-CHECK: ret
}

// ASM-CHECK:_TTSf2d_i_n___TFF9linetable4mainFSiT_U_FT_T_:
// ASM-CHECK-NOT: retq
// The end-of-prologue should have a valid location.
// ASM-CHECK: .loc	[[FILEID]] 37 {{[0-9]+}} prologue_end


main(30)
