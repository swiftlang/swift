// REQUIRES: swift_feature_CoroutineAccessors
// RUN: %target-swift-frontend -enable-experimental-feature CoroutineAccessors -Xllvm -sil-full-demangle %s -emit-irgen -g -o - | %FileCheck %s

func USE(_ int: inout Int) {}

struct S {
  private var member_int: Int = 0

  var computed_property: Int {
    get { return member_int }
    yielding mutate {
      yield &member_int
    }
  }
}

var state = S()
USE(&state.computed_property)

// CHECK-LABEL: define {{.*}} @{{.*}}computed_property
// CHECK-NOT: define
// CHECK: coro.end:
// CHECK-NEXT:  call {{.*}} @llvm.coro.end{{.*}} !dbg ![[LINE_ZERO:.*]]
// CHECK-NEXT:  unreachable, !dbg ![[LINE_ZERO]]


// CHECK: ![[LINE_ZERO]] = !DILocation(line: 0,
