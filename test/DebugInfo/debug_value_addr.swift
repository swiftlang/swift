// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-sil -g -o - | %FileCheck -check-prefix=CHECK-SIL %s

// Verify that -Onone shadow copies are emitted for debug_value_addr
// instructions.

// CHECK-SIL: sil hidden @$s16debug_value_addr4testyyxlF
// CHECK-SIL: debug_value_addr %0 : $*T, let, name "t"

// CHECK: define {{.*}}$s16debug_value_addr4testyyxlF
// CHECK: entry:
// CHECK-NEXT: %[[TADDR:.*]] = alloca
// CHECK-NEXT: call void @llvm.dbg.declare({{.*}}%[[TADDR]]
// CHECK: store %swift.opaque* %0, %swift.opaque** %[[TADDR:.*]], align

struct S<T> {
  var a : T
  func foo() {}
}

func test<T>(_ t : T) {
  let a = S(a: t)
  a.foo()
}
