// RUN: %target-swift-frontend %use_no_opaque_pointers %s -Onone -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o -
// RUN: %target-swift-frontend %use_no_opaque_pointers %s -Onone -emit-ir -g -o - \
// RUN:   -disable-debugger-shadow-copies | %FileCheck %s --check-prefix=NOCOPY
// RUN: %target-swift-frontend %s -Onone -emit-ir -g -o - \
// RUN:   -disable-debugger-shadow-copies
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
    // NOCOPY:      @"$s{{.*}}6ClassBCyACs5Int64Vcfc"
    // CHECK:       alloca {{.*}}ClassBC*
    // NOCOPY:      alloca {{.*}}ClassBC*

    // CHECK:       alloca i64

    // CHECK-NOT:   alloca
    // NOCOPY-NOT:  alloca
    // CHECK:       ret {{.*}}ClassBC
    // NOCOPY:      ret {{.*}}ClassBC
        super.init (input)
    }
}

let b = ClassB(1);

func use(_ x: Int) {}

class ClassC
{
    // CHECK:  define {{.*}}@"$s13shadow_copies6ClassCCACycfc"
    // NOCOPY: define {{.*}}@"$s13shadow_copies6ClassCCACycfc"
    init ()
    {
    // CHECK:  alloca %T13shadow_copies6ClassCC*
    // CHECK-NOT: alloca
    // NOCOPY-NOT: alloca

    // CHECK:  call void @llvm.dbg.value(metadata i{{(64|32)}} 10
    // NOCOPY: call void @llvm.dbg.value(metadata i{{(64|32)}} 10
        let x = 10

        use(x)

        use(x)

    // CHECK:  ret
    // NOCOPY: ret
    }
}

let c = ClassC()
