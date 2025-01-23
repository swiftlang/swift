// RUN: not %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded 2>&1 | %FileCheck %s --check-prefix CHECK-EXISTENTIALS
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s --check-prefix CHECK-TRAPS-SIL
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s --check-prefix CHECK-TRAPS-IR

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

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

// CHECK-EXISTENTIALS: error: cannot use a value of protocol type 'any Error' in embedded Swift

// CHECK-TRAPS-SIL:      sil @$e4main9throwing1SiyKF : $@convention(thin) () -> (Int, @error any Error) {
// CHECK-TRAPS-SIL-NEXT: bb0:
// CHECK-TRAPS-SIL-NEXT:   debug_value
// CHECK-TRAPS-SIL-NEXT:   %1 = integer_literal $Builtin.Int1, -1
// CHECK-TRAPS-SIL-NEXT:   cond_fail %1 : $Builtin.Int1, "throw turned into a trap"
// CHECK-TRAPS-SIL-NEXT:   unreachable
// CHECK-TRAPS-SIL-NEXT: }


// CHECK-TRAPS-IR:      define {{.*}}@"$e4main9throwing1SiyKF"{{.*}}{
// CHECK-TRAPS-IR-NEXT: entry:
// CHECK-TRAPS-IR:        call void @llvm.trap()
// CHECK-TRAPS-IR-NEXT:   unreachable
// CHECK-TRAPS-IR-NEXT: }
// CHECK-TRAPS-IR:      define {{.*}}@"$e4main9catching1yyF"{{.*}}{
