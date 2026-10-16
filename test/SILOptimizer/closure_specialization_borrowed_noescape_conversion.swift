// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name CS %s | %FileCheck %s

/// Removing the borrow before a nonthrowing-to-throwing conversion must enable closure specialization.

// CHECK-LABEL: {{^}}// specialized useThrowing(_:)
// CHECK:       sil shared [noinline] @[[SPECIALIZED:[^ ]+]] : $@convention(thin) (Int) -> (Int, @error any Error) {
// CHECK:       bb0(%[[#CAPTURE:]] : $Int):
// CHECK:         return %[[#CAPTURE]]
// CHECK-NEXT:  {{^}}} // end sil function
@inline(never)
func useThrowing(_ f: () throws -> Int) throws -> Int { try f() }

// CHECK-LABEL: {{^}}sil @$s2CS18throwingConversionyS2iKF :
// CHECK:       bb0(%[[#N:]] : $Int):
// CHECK-NOT:     partial_apply
// CHECK:         %[[#USE:]] = function_ref @[[SPECIALIZED]] : $@convention(thin) (Int) -> (Int, @error any Error)
// CHECK-NEXT:    try_apply %[[#USE]](%[[#N]]) : $@convention(thin) (Int) -> (Int, @error any Error), normal bb1, error bb2
// CHECK:       bb1(%[[#RESULT:]] : $Int):
// CHECK-NEXT:    return %[[#RESULT]]
// CHECK:       bb2(%[[#ERROR:]] : $any Error):
// CHECK-NEXT:    throw %[[#ERROR]]
// CHECK-NEXT:  {{^}}} // end sil function '$s2CS18throwingConversionyS2iKF'
public func throwingConversion(_ n: Int) throws -> Int {
  let f = { n }
  return try useThrowing(f)
}
