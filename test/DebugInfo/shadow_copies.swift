// RUN: %swift -target x86_64-apple-macosx10.9 %s -Onone -emit-ir -g -o - | FileCheck %s
class ClassA
{
    var x : Int
    var y : Float

    init (_ input : Int)
    {
        x = input
        y = Float(input) + 0.5
    }
}

class ClassB : ClassA
{
    override init (_ input : Int)
    {
    // CHECK: @_TFC{{.*}}6ClassBcfMS0_FSiS0_
    // CHECK:  alloca {{.*}}ClassB*
    // CHECK:  alloca i64
    // CHECK-NOT: alloca
    // CHECK: ret {{.*}}ClassB
        super.init (input)
    }
}

let b = ClassB(1);
