// RUN: %target-swift-emit-silgen %s -enable-experimental-feature TypedThrows | %FileCheck %s

enum MyError: Error {
  case fail
  case epicFail
}

enum MyBigError: Error {
  case epicFail
}

func throwsMyBigError() throws(MyBigError) { }

// CHECK: sil hidden [ossa] @$s12typed_throws20doesNotThrowConcreteyyAA7MyErrorOYKF : $@convention(thin) () -> @error MyError
func doesNotThrowConcrete() throws(MyError) { }

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws0B8ConcreteyyAA7MyErrorOYKF : $@convention(thin) () -> @error MyError
func throwsConcrete() throws(MyError) {
  // CHECK: [[ERROR:%[0-9]+]] = enum $MyError, #MyError.fail!enumelt
  // CHECK-NOT: builtin "willThrow"
  // CHECK: throw [[ERROR]] : $MyError
  throw .fail
}

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws15rethrowConcreteyyAA7MyErrorOYKF
func rethrowConcrete() throws(MyError) {
  // CHECK: try_apply [[FN:%[0-9]+]]() : $@convention(thin) () -> @error MyError, normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
  // CHECK: [[ERRORBB]]([[ERROR:%[0-9]+]] : $MyError)
  // CHECK-NEXT: throw [[ERROR]] : $MyError 
  try throwsConcrete()
}
// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws29rethrowConcreteAndExistentialyyKF
func rethrowConcreteAndExistential() throws {
  // CHECK: try_apply [[FN:%[0-9]+]]() : $@convention(thin) () -> @error MyError, normal [[NORMALBB:bb[0-9]+]], error [[ERRORBB:bb[0-9]+]]
  // CHECK: alloc_existential_box $any Error, $MyError
  // CHECK: throw [[ERROR:%[0-9]+]] : $any Error
  try throwsConcrete()
}


func sink<T: Error>(_: T) { }

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws0B27OneOrTheOtherWithoutRethrowyyF
func throwsOneOrTheOtherWithoutRethrow() {
  do {
    // FIXME: the generated code below could probably be better, because it
    // currently depends on injecting into an existential error.

    // CHECK: try_apply [[F1:%[0-9]+]]() : $@convention(thin) () -> @error MyError, normal [[F1_NORMAL:bb[0-9]+]], error [[F1_ERROR:bb[0-9]+]]
    try doesNotThrowConcrete()

    // CHECK: try_apply [[F2:%[0-9]+]]() : $@convention(thin) () -> @error MyError, normal [[F2_NORMAL:bb[0-9]+]], error [[F2_ERROR:bb[0-9]+]]
    try throwsConcrete()
    try throwsMyBigError()
  } catch let e as MyError {
    sink(e)
  } catch let be as MyBigError {
    sink(be)
  } catch {
    fatalError("not actually reachable")
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws0B24OneOrTheOtherWithRethrowyyKF
func throwsOneOrTheOtherWithRethrow() throws {
  do {
    // FIXME: This probably shouldn't even have a rethrow block, because the
    // type checker Should Know that this list of catch clauses is exhaustive.
    // For now, we check that there is a basic block that takes an existential
    // error, but we want that to go away.
    //
    // CHECK: return [[RET:%[0-9]+]] : $()
    // CHECK-NOT: return
    // CHECK: @owned $any Error
    // CHECK: throw
    try throwsConcrete()
    try throwsMyBigError()
  } catch let e as MyError {
    sink(e)
  } catch let be as MyBigError {
    sink(be)
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws0B26ConcreteWithDoCatchRethrowyyKF : $@convention(thin) () -> @error any Error
func throwsConcreteWithDoCatchRethrow() throws {
  do {
    // CHECK: [[FN:%[0-9]+]] = function_ref @$s12typed_throws0B8ConcreteyyAA7MyErrorOYKF : $@convention(thin) () -> @error MyError
    // CHECK: try_apply [[FN]]() : $@convention(thin) () -> @error MyError, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try throwsConcrete()

    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : $MyError):
    // CHECK-NEXT: switch_enum [[ERROR]] : $MyError, case #MyError.fail!enumelt: [[FAILCASE_BB:bb[0-9]+]], default [[DEFAULT_BB:bb[0-9]+]]
  } catch .fail {
  }

  // CHECK: [[DEFAULT_BB]]:
  // CHECK-NOT: throw
  // CHECK: alloc_existential_box $any Error
  // CHECK: throw [[ERR:%[0-9]+]] : $any Error
}

// CHECK-LABEL: sil hidden [ossa] @$s12typed_throws0B31ConcreteWithDoCatchTypedRethrowyyAA7MyErrorOYKF : $@convention(thin) () -> @error MyError
func throwsConcreteWithDoCatchTypedRethrow() throws(MyError) {
  do {
    // CHECK: [[FN:%[0-9]+]] = function_ref @$s12typed_throws0B8ConcreteyyAA7MyErrorOYKF : $@convention(thin) () -> @error MyError
    // CHECK: try_apply [[FN]]() : $@convention(thin) () -> @error MyError, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
    try throwsConcrete()

    // CHECK: [[ERROR_BB]]([[ERROR:%[0-9]+]] : $MyError):
    // CHECK-NEXT: switch_enum [[ERROR]] : $MyError, case #MyError.fail!enumelt: [[FAILCASE_BB:bb[0-9]+]], default [[DEFAULT_BB:bb[0-9]+]]
  } catch .fail {
  }

  // CHECK: [[DEFAULT_BB]]:
  // CHECK-NEXT: throw [[ERROR]] : $MyError
}

open class MyClass {
  // CHECK-LABEL: sil [serialized] [exact_self_class] [ossa] @$s12typed_throws7MyClassC4bodyACyyxYKXE_txYKcs5ErrorRzlufC : $@convention(method) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, @thick MyClass.Type) -> (@owned MyClass, @error_indirect E)
  // CHECK: bb0(%0 : $*E, %1 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, %2 : $@thick MyClass.Type):
  // CHECK: try_apply{{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  // CHECK: [[NORMAL_BB]]([[SELF:%.*]] : @owned $MyClass):
  // CHECK-NEXT: return [[SELF]] : $MyClass
  // CHECK: [[ERROR_BB]]:
  // CHECK-NEXT: throw_addr
  public init<E>(body: () throws(E) -> Void) throws(E) {
  }
}
