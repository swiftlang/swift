// REQUIRES: swift_feature_CoroutineAccessors
// RUN: %target-swift-frontend -enable-experimental-feature CoroutineAccessors -Xllvm -sil-full-demangle %s -emit-irgen -g -o - | %FileCheck %s

func USE(_ int: inout Int) {}
func FINISH() {}

struct S {
  private var member_int: Int = 0

  var computed_property: Int {
    get { return member_int }
    yielding mutate {
      yield &member_int
    }
  }
}

// CHECK-LABEL: define {{.*}}main{{.*}}(
var state = S()

// CHECK: %[[coro:.*]] = call ptr @llvm.coro.prepare.retcon
// CHECK: %[[call_result:.*]] = call swiftcc { ptr, ptr } %[[coro]]
// CHECK: %[[continuation_ptr:.*]] = extractvalue  { ptr, ptr } %[[call_result]]
// CHECK: call {{.*}} @{{.*}}USE{{.*}}, !dbg ![[DBG:.*]]
// CHECK: call swiftcc void %[[continuation_ptr]]{{.*}}, !dbg ![[DBG]]
USE(&state.computed_property)
FINISH()
// CHECK: call swiftcc void @"{{.*}}FINISH


// CHECK-LABEL: define {{.*}} @{{.*}}computed_property
// CHECK-SAME:  ptr swiftself {{.*}} %[[self:.*]])
// CHECK: %[[self_alloca:.*]] = alloca ptr
// CHECK: #dbg_declare(ptr %[[self_alloca]], ![[SELF_DBG:.*]], !DIExpression(DW_OP_deref)
// CHECK: call {{.*}} @llvm.coro.begin
// CHECK: store ptr %[[self]], ptr %[[self_alloca]]
// CHECK: call void asm sideeffect "", "r"(ptr %[[self_alloca]])
// CHECK: call {{.*}} @llvm.coro.suspend.retcon
// CHECK: call void asm sideeffect "", "r"(ptr %[[self_alloca]])
// CHECK: call {{.*}} @llvm.coro.end


// CHECK: ![[DBG]] = !DILocation(line: [[@LINE-17]],
