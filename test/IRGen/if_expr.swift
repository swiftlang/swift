// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

// CHECK: define i64 @_T7if_expr6scalarFT1bSb_Si
func scalar(b:Bool) -> Int {
  return if b
    then 123
    else 321
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE:.*]], label %[[IF_FALSE:.*]]
  // CHECK: [[IF_TRUE]]:
  // CHECK:   br label %[[CONT:.*]]
  // CHECK: [[IF_FALSE]]:
  // CHECK:   br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK:   phi i64 [ 123, %[[IF_TRUE]] ], [ 321, %[[IF_FALSE]] ]
}
// CHECK: }

// CHECK: define {{.*}} @_T7if_expr5tupleFT1bSb_TSiSc_
func tuple(b:Bool) -> (Int, Char) {
  return if b
    then (1234, 'X')
    else (4321, 'Y')
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE:.*]], label %[[IF_FALSE:.*]]
  // CHECK: [[IF_TRUE]]:
  // CHECK:   [[TRUE_CHAR:%.*]] = call i32 {{@.*}}(i32 88)
  // CHECK:   br label %[[CONT:.*]]
  // CHECK: [[IF_FALSE]]:
  // CHECK:   [[FALSE_CHAR:%.*]] = call i32 {{@.*}}(i32 89)
  // CHECK:   br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK:   phi i64 [ 1234, %[[IF_TRUE]] ], [ 4321, %[[IF_FALSE]] ]
  // CHECK:   phi i32 [ [[TRUE_CHAR]], %[[IF_TRUE]] ], [ [[FALSE_CHAR]], %[[IF_FALSE]] ]
}
// CHECK: }

// CHECK: define {{.*}} @_T7if_expr10releasableFT1bSb1sSS_SS
func releasable(b:Bool, s:String) -> String {
  return if b
    then s
    else "string"
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE:.*]], label %[[IF_FALSE:.*]]
  // CHECK: [[IF_TRUE]]:
  // CHECK:   br label %[[CONT:.*]]
  // CHECK: [[IF_FALSE]]:
  // CHECK:   br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK:   [[REFCTD:%.*]] = phi %swift.refcounted* [ {{%.*}}, %[[IF_TRUE]] ], [ {{%.*}}, %[[IF_FALSE]] ]
  // CHECK-NOT: call void @swift_release(%swift.refcounted* [[REFCTD]])
}
// CHECK: }

// CHECK: define void @_T7if_expr18releasable_ignoredFT1bSb1sSS_T_(i1 %b, i8* %s.0, i64 %s.1, %swift.refcounted* %s.2) {
func releasable_ignored(b:Bool, s:String) {
  if b
    then s
    else "string"
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE:.*]], label %[[IF_FALSE:.*]]
  // CHECK: [[IF_TRUE]]:
  // CHECK:   br label %[[CONT:.*]]
  // CHECK: [[IF_FALSE]]:
  // CHECK:   br label %[[CONT]]
  // CHECK: [[CONT]]:
  // CHECK:   [[REFCTD:%.*]] = phi %swift.refcounted* [ {{%.*}}, %[[IF_TRUE]] ], [ {{%.*}}, %[[IF_FALSE]] ]
  // CHECK: call void @swift_release(%swift.refcounted* [[REFCTD]])
}
// CHECK: }

// CHECK: define {{.*}} @_T7if_expr5chainFT1bSb1cSb_SS(i1 %b, i1 %c) {
func chain(b:Bool, c:Bool) -> String {
  return if b
      then "bees?!"
    else if c
      then "get rid of the seaward"
    else
      "i've made a huge mistake"
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE_1:.*]], label %[[IF_FALSE_1:.*]]
  // CHECK: [[IF_TRUE_1]]:
  // CHECK:   br label %[[CONT_1:.*]]
  // CHECK: [[IF_FALSE_1]]:
  // CHECK:   br i1 {{%.*}}, label %[[IF_TRUE_2:.*]], label %[[IF_FALSE_2:.*]]
  // CHECK: [[IF_TRUE_2]]:
  // CHECK:   br label %[[CONT_2:.*]]
  // CHECK: [[IF_FALSE_2]]:
  // CHECK:   br label %[[CONT_2:.*]]
  // CHECK: [[CONT_2]]:
  // CHECK:   phi {{.*}} [ {{.*}}, %[[IF_TRUE_2]] ], [ {{.*}}, %[[IF_FALSE_2]] ]
  // CHECK:   br label %[[CONT_1:.*]]
  // CHECK: [[CONT_1]]:
  // CHECK:   phi {{.*}} [ {{.*}}, %[[IF_TRUE_1]] ], [ {{.*}}, %[[CONT_2]] ]
}
// CHECK: }
