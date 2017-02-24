// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -Onone -emit-ir -g -o - | %FileCheck %s

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
    // CHECK: @_T0{{.*}}6ClassBCACs5Int64Vcfc
    // CHECK:  alloca {{.*}}ClassBC*
    // CHECK:  alloca i64
    // CHECK-NOT: alloca
    // CHECK: ret {{.*}}ClassBC
        super.init (input)
    }
}

let b = ClassB(1);
