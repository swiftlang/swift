// RUN: %target-swift-frontend -emit-sil -verify %s | FileCheck %s

func rethrower(fn: () throws -> Int) rethrows -> Int {
  return try fn()
}
func thrower() throws -> Int { return 0 }
func nonthrower() -> Int { return 0 }

// CHECK-LABEL: sil hidden @_TF8rethrows5test0FzT_T_ : $@convention(thin) () -> @error ErrorType {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_TF8rethrows9rethrowerFzFzT_SiSi : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType)
// CHECK:       [[THROWER:%.*]] = function_ref @_TF8rethrows7throwerFzT_Si : $@convention(thin) () -> (Int, @error ErrorType)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $ErrorType):
// CHECK-NEXT:  throw [[T0]]
func test0() throws {
  try rethrower(thrower)
}

// CHECK-LABEL: sil hidden @_TF8rethrows5test1FzT_T_ : $@convention(thin) () -> @error ErrorType {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_TF8rethrows9rethrowerFzFzT_SiSi : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType)
// CHECK:       [[CLOSURE:%.*]] = function_ref @_TFF8rethrows5test1FzT_T_U_FzT_Si : $@convention(thin) () -> (Int, @error ErrorType)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[CLOSURE]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $ErrorType):
// CHECK-NEXT:  throw [[T0]]
//   Closure.
// CHECK-LABEL: sil shared @_TFF8rethrows5test1FzT_T_U_FzT_Si : $@convention(thin) () -> (Int, @error ErrorType) {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_TF8rethrows9rethrowerFzFzT_SiSi : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType)
// CHECK:       [[THROWER:%.*]] = function_ref @_TF8rethrows7throwerFzT_Si : $@convention(thin) () -> (Int, @error ErrorType)
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[THROWER]]
// CHECK:       try_apply [[RETHROWER]]([[T0]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  return [[T0]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $ErrorType):
// CHECK-NEXT:  throw [[T0]]
func test1() throws {
  try rethrower { try rethrower(thrower) }
}

// CHECK-LABEL: sil hidden @_TF8rethrows5test2FT_T_ : $@convention(thin) () -> () {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_TF8rethrows9rethrowerFzFzT_SiSi : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType)
// CHECK:       [[NONTHROWER:%.*]] = function_ref @_TF8rethrows10nonthrowerFT_Si : $@convention(thin) () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[NONTHROWER]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@callee_owned () -> Int to $@callee_owned () -> (Int, @error ErrorType)
// CHECK:       try_apply [[RETHROWER]]([[T1]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $ErrorType):
// CHECK-NEXT:  unreachable
func test2() {
  rethrower(nonthrower)
}

// CHECK-LABEL: sil hidden @_TF8rethrows5test3FT_T_ : $@convention(thin) () -> () {
// CHECK:       [[RETHROWER:%.*]] = function_ref @_TF8rethrows9rethrowerFzFzT_SiSi : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType)
// CHECK:       [[CLOSURE:%.*]] = function_ref @_TFF8rethrows5test3FT_T_U_FT_Si : $@convention(thin) () -> Int
// CHECK:       [[T0:%.*]] = thin_to_thick_function [[NONTHROWER]]
// CHECK:       [[T1:%.*]] = convert_function [[T0]] : $@callee_owned () -> Int to $@callee_owned () -> (Int, @error ErrorType)
// CHECK:       try_apply [[RETHROWER]]([[T1]]) : $@convention(thin) (@owned @callee_owned () -> (Int, @error ErrorType)) -> (Int, @error ErrorType), normal [[NORMAL:bb1]], error [[ERROR:bb2]]
// CHECK:     [[NORMAL]]([[T0:%.*]] : $Int):
// CHECK-NEXT:  [[T1:%.*]] = tuple ()
// CHECK-NEXT:  return [[T1]]
// CHECK:     [[ERROR]]([[T0:%.*]] : $ErrorType):
// CHECK-NEXT:  unreachable
func test3() {
  rethrower { rethrower(nonthrower) }
}
