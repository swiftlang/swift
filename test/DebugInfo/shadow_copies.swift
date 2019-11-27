// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - \
// RUN:   -disable-debugger-shadow-copies | %FileCheck %s --check-prefix=NOCOPY
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
    // CHECK:       @"$s{{.*}}6ClassBCyACs5Int64Vcfc"
    // NOPCOPY:     @"$s{{.*}}6ClassBCyACs5Int64Vcfc"
    // CHECK:       alloca {{.*}}ClassBC*
    // NOPCOPY:     alloca {{.*}}ClassBC*

    // CHECK:       alloca i64

    // CHECK-NOT:   alloca
    // NOPCOPY-NOT: alloca
    // CHECK:       ret {{.*}}ClassBC
    // NOCOPY:      ret {{.*}}ClassBC
        super.init (input)
    }
}

let b = ClassB(1);
