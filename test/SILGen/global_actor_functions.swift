// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5  -disable-availability-checking | %FileCheck --enable-var-scope %s
// REQUIRES: concurrency

actor MyActor { }

@globalActor
struct GlobalActor {
  static var shared: MyActor = MyActor()
}

@GlobalActor
class Super {
  func f() { }
}

@GlobalActor
class Sub: Super {
  // CHECK-LABEL: sil hidden [ossa] @$s4test3SubC1fyyF : $@convention(method) (@guaranteed Sub) -> ()
  // CHECK: function_ref @$s4test5SuperC1fyyF
  // CHECK-NEXT: apply
  override func f() {
    super.f()
  }
}
