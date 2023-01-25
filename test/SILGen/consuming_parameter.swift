// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func bar(_: String) {}

// CHECK-LABEL: sil {{.*}} @${{.*}}3foo
func foo(y: consuming String, z: String) -> () -> String {
    // CHECK: bb0(%0 : @owned $String, %1 : @guaranteed $String):
    // CHECK:   [[BOX:%.*]] = alloc_box ${ var String }
    // CHECK:   [[BOX0:%.*]] = mark_uninitialized [var] [[BOX]]
    // CHECK:   [[BOX1:%.*]] = begin_borrow [lexical] [[BOX0]]
    // CHECK:   [[Y:%.*]] = project_box [[BOX1]]
    // CHECK:   store %0 to [init] [[Y]]

    // CHECK:   [[YCAPTURE:%.*]] = copy_value [[BOX1]]
    // CHECK:   partial_apply {{.*}} {{%.*}}([[YCAPTURE]])
    let r = { y }

    // CHECK:   [[ZCOPY:%.*]] = copy_value %1
    // CHECK:   [[YACCESS:%.*]] = begin_access [modify] [unknown] [[Y]]
    // CHECK:   assign [[ZCOPY]] to [[YACCESS]]
    y = z

    // CHECK:   [[YACCESS:%.*]] = begin_access [read] [unknown] [[Y]]
    // CHECK:   [[YVAL:%.*]] = load [copy] [[YACCESS]]
    // CHECK:   apply {{%.*}}([[YVAL]]
    bar(y)

    return r
}
