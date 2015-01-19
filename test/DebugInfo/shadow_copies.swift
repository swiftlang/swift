// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - | FileCheck %s

class ClassA
{
    var x : Int64
    var y : Float

    init (_ input : Int64)
    {
        x = input
        y = Float(input) + 0.5
    }
}

class ClassB : ClassA
{
    override init (_ input : Int64)
    {
    // CHECK: @_TFC{{.*}}6ClassBcfMS0_FVSs5Int64S0_
    // CHECK:  alloca {{.*}}ClassB*
    // CHECK:  alloca i64
    // CHECK-NOT: alloca
    // CHECK: ret {{.*}}ClassB
        super.init (input)
    }
}

let b = ClassB(1);
