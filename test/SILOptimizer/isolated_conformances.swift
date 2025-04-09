// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -parse-as-library -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --check-prefix=CHECK-OUTPUT

// RUN: %target-build-swift -parse-as-library -O %s -Xllvm -sil-disable-pass=function-signature-opts -emit-sil | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx || OS=linux-gnu

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

protocol P {
  func f() -> Int
}

struct X: @MainActor P {
  func f() -> Int { 17 }
}

// CHECK-LABEL: sil hidden [noinline] @$s21isolated_conformances12castAnywhereySiSgAA1XVF :
// CHECK:         checked_cast_addr_br take_always X in %{{[0-9]+}} to any P in %{{[0-9]+}}, bb1, bb2
// CHECK:       } // end sil function '$s21isolated_conformances12castAnywhereySiSgAA1XVF'
@inline(never)
nonisolated func castAnywhere(_ value: X) -> Int? {
  if let value = value as? P {
    return value.f()
  }
  return nil
}

// CHECK-LABEL: sil hidden [noinline] @$s21isolated_conformances15castOnMainActorySiSgAA1XVF :
// CHECK:         [[L:%.*]] = integer_literal {{.*}} 17
// CHECK:         [[I:%.*]] = struct $Int ([[L]])
// CHECK:         [[O:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[I]]
// CHECK:         return [[O]]
// CHECK:       } // end sil function '$s21isolated_conformances15castOnMainActorySiSgAA1XVF'
@MainActor
@inline(never)
func castOnMainActor(_ value: X) -> Int? {
  if let value = value as? P {
    return value.f()
  }
  return nil
}


@main
struct Main {
  static func main() async {
    await Task.detached {
      // CHECK-OUTPUT: nil
      print(castAnywhere(X()))
    }.value
  }
}
