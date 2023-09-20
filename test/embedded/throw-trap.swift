// RUN: not %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded 2>&1 | %FileCheck %s --check-prefix CHECK-EXISTENTIALS
// RUN: %target-swift-frontend -emit-sil %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s --check-prefix CHECK-TRAPS-SIL
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s --check-prefix CHECK-TRAPS-IR

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

enum MyError : Error {
  case a
}

public func throwing1() throws -> Int {
  throw MyError.a
}

public func catching1() {
  do {
    try throwing1()
  } catch let e as MyError {
    _ = e
  } catch {
    _ = error
  }
}

// CHECK-EXISTENTIALS: error: existential can cause metadata allocation or locks

// CHECK-TRAPS-SIL:      sil @$s4main9throwing1SiyKF : $@convention(thin) () -> (Int, @error any Error) {
// CHECK-TRAPS-SIL-NEXT: bb0:
// CHECK-TRAPS-SIL-NEXT:   debug_value
// CHECK-TRAPS-SIL-NEXT:   %1 = builtin "int_trap"
// CHECK-TRAPS-SIL-NEXT:   unreachable
// CHECK-TRAPS-SIL-NEXT: }


// CHECK-TRAPS-IR:      define {{.*}}@"$s4main9throwing1SiyKF"{{.*}}{
// CHECK-TRAPS-IR-NEXT: entry:
// CHECK-TRAPS-IR-NEXT:   call void @llvm.trap()
// CHECK-TRAPS-IR-NEXT:   unreachable
// CHECK-TRAPS-IR-NEXT: }
