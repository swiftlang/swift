// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

func foo(x: borrowing String) {}

var last:String
{
    // CHECK-LABEL: sil{{.*}} @{{.*}}4lastSSvr :
    _read
    {
        // CHECK: [[X:%.*]] = move_value
        let x:String = ""
        // CHECK:   [[BORROW1:%.*]] = begin_borrow [[X]]
        // CHECK:   apply {{.*}}([[BORROW1]])
        // CHECK:   end_borrow [[BORROW1]]
        foo(x: x)
        // CHECK:   [[BORROW2:%.*]] = begin_borrow [[X]]
        // CHECK:   yield [[BORROW2]] : {{.*}}, resume [[RESUME:bb[0-9]+]],
        // CHECK: [[RESUME]]:
        // CHECK:   end_borrow [[BORROW2]]
        yield x
    }

    _modify {
        var x: String = ""
        yield &x
    }
}
