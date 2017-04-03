// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

@discardableResult
func rethrower(_ fn: () throws -> Int) rethrows -> Int {
  return try fn()
}
func thrower() throws -> Int { return 0 }
func nonthrower() -> Int { return 0 }

// CHECK-LABEL: sil hidden @_T08rethrows5test0yyKF : $@convention(thin) () -> @error Error {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_T08rethrows9rethrowerS2iyKcKF : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       [[THROWER:%.*]] = function_ref @_T08rethrows7throwerSiyKF : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  throw [[T0]]
func test0() throws {
  try rethrower(thrower)
}

// CHECK-LABEL: sil hidden @_T08rethrows5test1yyKF : $@convention(thin) () -> @error Error {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_T08rethrows9rethrowerS2iyKcKF : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       [[CLOSURE:%.*]] = function_ref @_T08rethrows5test1yyKFSiyKcfU_ : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[CLOSURE]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  throw [[T0]]
//   Closure.
// CHECK-LABEL: sil private @_T08rethrows5test1yyKFSiyKcfU_ : $@convention(thin) () -> (Int, @error Error) {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_T08rethrows9rethrowerS2iyKcKF : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       [[THROWER:%.*]] = function_ref @_T08rethrows7throwerSiyKF : $@convention(thin) () -> (Int, @error Error)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  return [[T0]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  throw [[T0]]
func test1() throws {
  try rethrower { try rethrower(thrower) }
}

// CHECK-LABEL: sil hidden @_T08rethrows5test2yyF : $@convention(thin) () -> () {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_T08rethrows9rethrowerS2iyKcKF : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       [[NONTHROWER:%.*]] = function_ref @_T08rethrows10nonthrowerSiyF : $@convention(thin) () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[NONTHROWER]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@callee_owned () -> Int to $@callee_owned () -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[T1]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  unreachable
func test2() {
  rethrower(nonthrower)
}

// CHECK-LABEL: sil hidden @_T08rethrows5test3yyF : $@convention(thin) () -> () {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_T08rethrows9rethrowerS2iyKcKF : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error)
// CHECK:       [[CLOSURE:%.*]] = function_ref @_T08rethrows5test3yyFSiycfU_ : $@convention(thin) () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[NONTHROWER]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@callee_owned () -> Int to $@callee_owned () -> (Int, @error Error)
// CHECK:       try_apply [[RETHROWER]]([[T1]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error Error)) -> (Int, @error Error), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $Error):
// CHECK-NEXT:  unreachable
func test3() {
  rethrower { rethrower(nonthrower) }
}
