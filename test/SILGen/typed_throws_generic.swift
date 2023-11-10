// RUN: %target-swift-emit-silgen %s -enable-experimental-feature TypedThrows -enable-experimental-feature FullTypedThrows | %FileCheck %s

public func genericThrow<E>(e: E) throws(E) {
  throw e
}


// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C5Throw1eyx_txYKs5ErrorRzlF : $@convention(thin) <E where E : Error> (@in_guaranteed E) -> @error_indirect E {

// CHECK: bb0(%0 : $*E, %1 : $*E):
// CHECK: [[TMP:%.*]] = alloc_stack $E
// CHECK: copy_addr %1 to [init] [[TMP]] : $*E
// CHECK: copy_addr [take] [[TMP]] to [init] %0 : $*E
// CHECK: dealloc_stack [[TMP]] : $*E
// CHECK: throw_addr


public func genericTryApply<E>(fn: () throws(E) -> ()) throws(E) {
  try fn()
}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C8TryApply2fnyyyxYKXE_txYKs5ErrorRzlF : $@convention(thin) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>) -> @error_indirect E {

// CHECK: bb0(%0 : $*E, %1 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>):
// CHECK: [[FN:%.*]] = copy_value %1 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: [[ERROR:%.*]] = alloc_stack $E
// CHECK: [[FN_BORROW:%.*]] = begin_borrow [[FN]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: try_apply [[FN_BORROW]]([[ERROR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, normal bb1, error bb2

// CHECK: bb1(%7 : $()):
// CHECK: end_borrow [[FN_BORROW]]
// CHECK: dealloc_stack [[ERROR]]
// CHECK: destroy_value [[FN]]
// CHECK: return

// CHECK: bb2:
// CHECK: copy_addr [take] [[ERROR]] to [init] %0 : $*E
// CHECK: end_borrow [[FN_BORROW]]
// CHECK: dealloc_stack [[ERROR]]
// CHECK: destroy_value [[FN]]
// CHECK: throw_addr


public func genericOptionalTry<E>(fn: () throws(E) -> ()) -> ()? {
  return try? fn()
}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C11OptionalTry2fnytSgyyxYKXE_ts5ErrorRzlF : $@convention(thin) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>) -> Optional<()> {
// CHECK: bb0(%0 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>):
// CHECK: [[FN:%.*]] = copy_value %0 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: [[ERROR:%.*]] = alloc_stack $E
// CHECK: [[FN_BORROW:%.*]] = begin_borrow %2 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: try_apply [[FN_BORROW]]([[ERROR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, normal bb1, error bb3

// CHECK: bb1({{.*}} : $()):
// CHECK: end_borrow [[FN_BORROW]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: dealloc_stack [[ERROR]] : $*E
// CHECK: [[RESULT:%.*]] = tuple ()
// CHECK: [[OPT:%.*]] = enum $Optional<()>, #Optional.some!enumelt, [[RESULT]] : $()
// CHECK: destroy_value [[FN]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: br bb2([[OPT]] : $Optional<()>)

// CHECK: bb2([[RESULT:%.*]] : $Optional<()>):
// CHECK: return [[RESULT]] : $Optional<()>

// CHECK: bb3:
// CHECK: destroy_addr [[ERROR]] : $*E
// CHECK: end_borrow [[FN_BORROW]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: dealloc_stack [[ERROR]] : $*E
// CHECK: destroy_value [[FN]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: [[RESULT:%.*]] = enum $Optional<()>, #Optional.none!enumelt
// CHECK: br bb2([[RESULT]] : $Optional<()>)


public func genericForceTry<E>(fn: () throws(E) -> ()) {
  try! fn()
}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C8ForceTry2fnyyyxYKXE_ts5ErrorRzlF : $@convention(thin) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>) -> () {
// CHECK: bb0(%0 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>):
// CHECK: [[FN:%.*]] = copy_value %0 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: [[ERROR:%.*]] = alloc_stack $E
// CHECK: [[FN_BORROW:%.*]] = begin_borrow %2 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: try_apply [[FN_BORROW]]([[ERROR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, normal bb1, error bb2

// CHECK: bb1({{.*}} : $()):
// CHECK: end_borrow [[FN_BORROW]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: dealloc_stack [[ERROR]] : $*E
// CHECK: [[RESULT:%.*]] = tuple ()
// CHECK: return [[RESULT]] : $()

// CHECK: bb2:
// CHECK: destroy_addr [[ERROR]] : $*E
// CHECK: end_borrow [[FN_BORROW]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: dealloc_stack [[ERROR]] : $*E
// CHECK: destroy_value [[FN]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: unreachable

enum MyError: Error {
case fail
}

func passthroughCall<T, E>(_ body: () throws(E) -> T) throws(E) -> T {
  try body()
}

func five() -> Int { 5 }

func fiveOrBust() throws -> Int { 5 }

func fiveOrTypedBust() throws(MyError) -> Int { 5 }

// CHECK-LABEL: sil hidden [ossa] @$s20typed_throws_generic23reabstractAsNonthrowingSiyF
func reabstractAsNonthrowing() -> Int {
  // CHECK: [[FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PRIOR_FN:%.*]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int> to $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int>
  // CHECK: [[CALLEE:%.*]] = function_ref @$s20typed_throws_generic15passthroughCallyxxyq_YKXEq_YKs5ErrorR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1)
  // CHECK: [[ERROR_SLOT:%.*]] = alloc_stack $Never
  // CHECK: try_apply [[CALLEE]]<Int, Never>(%0, [[ERROR_SLOT]], [[FN]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  passthroughCall(five)

  // CHECK: [[NORMAL_BB]]
  // CHECK: dealloc_stack [[ERROR_SLOT]] : $*Never

  // CHECK: [[ERROR_BB]]:
  // CHECK-NEXT: unreachable
}

// CHECK-LABEL: sil hidden [ossa] @$s20typed_throws_generic20reabstractAsThrowingSiyKF
func reabstractAsThrowing() throws -> Int {
  // CHECK: [[FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PRIOR_FN:%.*]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <any Error, Int> to $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <any Error, Int>
  // CHECK: [[CALLEE:%.*]] = function_ref @$s20typed_throws_generic15passthroughCallyxxyq_YKXEq_YKs5ErrorR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1)
  // CHECK: [[ERROR_SLOT:%.*]] = alloc_stack $any Error
  // CHECK: try_apply [[CALLEE]]<Int, any Error>(%1, [[ERROR_SLOT]], [[FN]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  try passthroughCall(fiveOrBust)

  // CHECK: [[NORMAL_BB]]
  // CHECK: dealloc_stack [[ERROR_SLOT]] : $*any Error

  // CHECK: [[ERROR_BB]]:
  // CHECK: throw [[ERR:%.*]] : $any Error
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSis5Error_pIgdzo_SisAA_pIegrzr_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> (Int, @error any Error)) -> (@out Int, @error_indirect any Error)
// CHECK: bb0([[ARG:%.*]] : $*Int, [[OUTER_ERROR_SLOT:%.*]] : $*any Error, [[INNER_FN:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> (Int, @error any Error)):
// CHECK: try_apply [[INNER_FN]]() : $@noescape @callee_guaranteed () -> (Int, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]

// CHECK: [[NORMAL_BB]]([[RESULT:%.*]] : $Int):
// CHECK-NEXT: store [[RESULT]] to [trivial] [[ARG]] : $*Int
// CHECK-NEXT: [[VOID_RESULT:%.*]] = tuple ()
// CHECK-NEXT:  return [[VOID_RESULT]] : $()

// CHECK: [[ERROR_BB]]([[INNER_ERROR:%.*]] : @owned $any Error):
// CHECK-NEXT:  store [[INNER_ERROR]] to [init] [[OUTER_ERROR_SLOT]] : $*any Error
// CHECK-NEXT:  throw_addr

// CHECK-LABEL: sil hidden [ossa] @$s20typed_throws_generic28reabstractAsConcreteThrowingSiyKF : $@convention(thin) () -> (Int, @error any Error)
func reabstractAsConcreteThrowing() throws -> Int {
  // CHECK: [[FN:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PRIOR_FN:%.*]] : $@callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <MyError, Int> to $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <MyError, Int>
  // CHECK: [[CALLEE:%.*]] = function_ref @$s20typed_throws_generic15passthroughCallyxxyq_YKXEq_YKs5ErrorR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1)
  // CHECK: [[ERROR_SLOT:%.*]] = alloc_stack $MyError
  // CHECK: try_apply [[CALLEE]]<Int, MyError>(%1, [[ERROR_SLOT]], [[FN]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  try passthroughCall(fiveOrTypedBust)

  // CHECK: [[NORMAL_BB]]
  // CHECK-NEXT: dealloc_stack [[ERROR_SLOT]] : $*MyError

  // CHECK: [[ERROR_BB]]:
  // CHECK: alloc_existential_box $any Error, $MyError
  // CHECK-NEXT: project_existential_box $MyError in
  // CHECK: throw [[ERR:%.*]] : $any Error
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSi20typed_throws_generic7MyErrorOIgdzo_SiACIegrzr_TR : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> (Int, @error MyError)) -> (@out Int, @error_indirect MyError) {
// CHECK: bb0([[RESULT_ADDR:%.*]] : $*Int, [[OUTER_ERROR:%.*]] : $*MyError, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> (Int, @error MyError)):
// CHECK-NEXT: try_apply [[FN]]() : $@noescape @callee_guaranteed () -> (Int, @error MyError), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]

// CHECK: [[NORMAL_BB]]([[INNER_RESULT:%.*]] : $Int):
// CHECK-NEXT: store [[INNER_RESULT]] to [trivial] [[RESULT_ADDR]] : $*Int

// CHECK: [[ERROR_BB]]([[INNER_ERROR:%.*]] : $MyError):
// CHECK-NEXT: store [[INNER_ERROR]] to [trivial] [[OUTER_ERROR]] : $*MyError
// CHECK-NEXT: throw_addr


// CHECK-LABEL: sil hidden [ossa] @$s20typed_throws_generic30reabstractClosureAsNonthrowingSiyF : $@convention(thin) () -> Int
func reabstractClosureAsNonthrowing() -> Int {
  // CHECK: [[INT_BOX:%.*]] = alloc_stack $Int
  // CHECK: [[CLOSURE:%.*]] = function_ref @$s20typed_throws_generic30reabstractClosureAsNonthrowingSiyFSiyXEfU_ : $@convention(thin) @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int>
  // CHECK-NEXT: [[THICK_CLOSURE:%.*]] = thin_to_thick_function [[CLOSURE]] : $@convention(thin) @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int> to $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int>
  // CHECK: [[CALLEE:%.*]] = function_ref @$s20typed_throws_generic15passthroughCallyxxyq_YKXEq_YKs5ErrorR_r0_lF : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1)
  // CHECK-NEXT: [[NEVER_BOX:%.*]] = alloc_stack $Never
  // CHECK-NEXT: try_apply [[CALLEE]]<Int, Never>([[INT_BOX]], [[NEVER_BOX]], [[THICK_CLOSURE]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  passthroughCall { 5 }

  // CHECK: [[NORMAL_BB]]
  // CHECK-NEXT: dealloc_stack [[NEVER_BOX]] : $*Never
  // CHECK-NEXT: [[RESULT:%.*]] = load [trivial] [[INT_BOX]] : $*Int
  // CHECK-NEXT: dealloc_stack [[INT_BOX]] : $*Int
  // CHECK-NEXT: return [[RESULT]] : $Int

  // CHECK: [[ERROR_BB]]:
  // CHECK-NEXT: unreachable

  // CHECK-LABEL: sil private [ossa] @$s20typed_throws_generic30reabstractClosureAsNonthrowingSiyFSiyXEfU_ : $@convention(thin) @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <Never, Int>
}

func reabstractClosureAsThrowing(b: Bool) throws -> Int {
  try passthroughCall {
    if b {
      throw MyError.fail
    }

    return 5
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s20typed_throws_generic32reabstractClosureAsTypedThrowing1bSiSb_tAA7MyErrorOYKF : $@convention(thin) (Bool) -> (Int, @error MyError)
func reabstractClosureAsTypedThrowing(b: Bool) throws(MyError) -> Int {
  // CHECK: try_apply [[CALLEE:%.*]]<Int, MyError>([[INT_BOX:%.*]], [[ERROR_BOX:%.*]], [[THICK_CLOSURE:%.*]]) : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_1 : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <τ_0_1, τ_0_0>) -> (@out τ_0_0, @error_indirect τ_0_1), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  try passthroughCall { () throws(MyError) -> Int in
    if b {
      throw MyError.fail
    }

    return 5
  }

  // CHECK: [[NORMAL_BB]]
  // CHECK-NEXT: dealloc_stack [[ERROR_BOX]] : $*MyError
  // CHECK: [[RESULT:%.*]] = load [trivial] [[INT_BOX]] : $*Int
  // CHECK-NEXT: dealloc_stack [[INT_BOX]] : $*Int
  // CHECK-NEXT: return [[RESULT]] : $Int

  // CHECK: [[ERROR_BB]]:
  // CHECK-NEXT: [[ERROR:%.*]] = load [trivial] [[ERROR_BOX]] : $*MyError
  // CHECK-NEXT: dealloc_stack [[ERROR_BOX]] : $*MyError
  // CHECK: dealloc_stack [[INT_BOX]] : $*Int
  // CHECK-NEXT: throw [[ERROR]] : $MyError
}


extension Collection {
  func typedMap<T, E>(body: (Element) throws(E) -> T) throws(E) -> [T] {
    var result: [T] = []
    for element in self {
      result.append(try body(element))
    }
    return result
  }
}

// CHECK-LABEL: sil private [ossa] @$s20typed_throws_generic9forcedMapySayq_GSayxGr0_lFq_xXEfU_ : $@convention(thin) <T, U> (@in_guaranteed T) -> (@out U, @error_indirect Never)
func forcedMap<T, U>(_ source: [T]) -> [U] {
  // CHECK: bb0(%0 : $*U, %1 : $*Never, %2 : $*T)
  return source.typedMap { $0 as! U }
}
