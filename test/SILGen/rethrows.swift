
// RUN: %target-swift-frontend -module-name rethrows -enable-sil-ownership -emit-sil -verify %s | %FileCheck %s

@discardableResult
func rethrower(_ fn: () throws -> Int) rethrows -> Int {
  return try fn()
}
func thrower() throws -> Int { return 0 }
func nonthrower() -> Int { return 0 }

// CHECK-LABEL: sil hidden @$S8rethrows5test0yyKF : $@convention(thin) () -> @error Error {
// CHECK:       [[THROWER:%.*]] = function_ref @$S8rethrows7throwerSiyKF : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       [[CVT:%.*]] = convert_escape_to_noescape [[T0]]
// CHECK:       [[RETHROWER:%.*]] = function_ref @$S8rethrows9rethroweryS2iyKXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[CVT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[RESULT_T0:%.*]] : $Int):
// FIXME - SR-6979: We should be able to eliminate this strong_release.
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[RESULT_T0:%.*]] : $Error):
// FIXME - SR-6979: We should be able to eliminate this strong_release.
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  throw [[RESULT_T0]]
func test0() throws {
  try rethrower(thrower)
}

// CHECK-LABEL: sil hidden @$S8rethrows5test1yyKF : $@convention(thin) () -> @error Error {
// CHECK:       [[CLOSURE:%.*]] = function_ref @$S8rethrows5test1yyKFSiyKXEfU_ : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[CVT:%.*]] = convert_function [[CLOSURE]]
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[CVT]]
// CHECK:       [[RETHROWER:%.*]] = function_ref @$S8rethrows9rethroweryS2iyKXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]({{%.*}} : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:  throw [[ERROR]]
//   Closure.
// CHECK-LABEL: sil private @$S8rethrows5test1yyKFSiyKXEfU_ : $@convention(thin) () -> (Int, @error Error) {
// CHECK:       [[THROWER:%.*]] = function_ref @$S8rethrows7throwerSiyKF : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       [[CVT:%.*]] = convert_escape_to_noescape [[T0]]
// CHECK:       [[RETHROWER:%.*]] = function_ref @$S8rethrows9rethroweryS2iyKXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[CVT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[RESULT:%.*]] : $Int):
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  return [[RESULT]]
// CHECK:     [[ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  strong_release [[T0]]
// CHECK-NEXT:  throw [[ERROR]]
func test1() throws {
  try rethrower { try rethrower(thrower) }
}

// CHECK-LABEL: sil hidden @$S8rethrows5test2yyF : $@convention(thin) () -> () {
// CHECK:       [[NONTHROWER:%.*]] = function_ref @$S8rethrows10nonthrowerSiyF : $@convention(thin) () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[NONTHROWER]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@callee_guaranteed () -> Int to $@callee_guaranteed () -> (Int, @error Error)
// CHECK:       [[T2:%.*]] = convert_escape_to_noescape [[T1]]
// CHECK:       [[RETHROWER:%.*]] = function_ref @$S8rethrows9rethroweryS2iyKXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[T2]]) : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  strong_release [[T1]]
// CHECK-NEXT:  strong_release [[T1]]
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  strong_release [[T1]]
// CHECK-NEXT:  unreachable
func test2() {
  rethrower(nonthrower)
}

// CHECK-LABEL: sil hidden @$S8rethrows5test3yyF : $@convention(thin) () -> () {
// CHECK:       [[CLOSURE:%.*]] = function_ref @$S8rethrows5test3yyFSiyXEfU_ : $@convention(thin) () -> Int
// CHECK:       [[CVT:%.*]] = convert_function [[CLOSURE]] : $@convention(thin) () -> Int to $@convention(thin) @noescape () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[CVT]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@noescape @callee_guaranteed () -> Int to $@noescape @callee_guaranteed () -> (Int, @error Error)
// CHECK:       [[RETHROWER:%.*]] = function_ref @$S8rethrows9rethroweryS2iyKXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[T1]]) : $@convention(thin) (@noescape @callee_guaranteed () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]({{%.*}} : $Int):
// CHECK-NEXT:  [[RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[RESULT]]
// CHECK:     [[ERROR]]([[ERROR:%.*]] : $Error):
// CHECK-NEXT:  unreachable
// CHECK-LABEL: // end sil function '$S8rethrows5test3yyF'
func test3() {
  rethrower { rethrower(nonthrower) }
}
