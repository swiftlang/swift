// RUN: %target-swift-emit-silgen -target %target-swift-5.1-abi-triple %s \
// RUN:   | %FileCheck %s
// RUN: %target-swift-emit-ir -target %target-swift-5.1-abi-triple %s \
// RUN:   -o /dev/null
// RUN: %target-swift-emit-ir -target %target-swift-5.1-abi-triple -O %s \
// RUN:   -o /dev/null

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/92312
//
// A closure literal with a typed throws clause that is passed to a generic
// parameter with an untyped `throws` is emitted directly at the parameter's
// abstraction pattern. Its body throws type-erased errors, so its lowered
// function type must use `@error any Error` as well. Lowering it with the
// concrete error type produced invalid SIL that crashed IRGen for errors with
// storage and returned corrupted errors for payload-less enums.

struct StringError: Error { var message: String }
struct OptionalError: Error { var code: Int? }
struct BoolError: Error { var flag: Bool }
struct RequestError: Error {
  let code: Int
  let message: String
}
enum EnumError: Error { case a, b, c }
final class ClassError: Error {}

func generic<T>(_ op: () throws -> T) {}
func genericAsync<T>(_ op: () async throws -> T) {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}10testStringyyF :
// CHECK-NOT:     thunk
// CHECK:         function_ref @$s{{.*}}10testStringyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int>
// CHECK-NOT:     thunk
// CHECK:       } // end sil function '$s{{.*}}10testStringyyF'
func testString() {
  genericAsync { () async throws(StringError) -> Int in
    throw StringError(message: "s")
  }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}10testStringyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         debug_value undef : $any Error, var, name "$error"
// CHECK:         alloc_existential_box $any Error, $StringError
// CHECK:         throw {{%.*}}
// CHECK:       } // end sil function '$s{{.*}}10testStringyyF{{.*}}fU_'

// CHECK-LABEL: sil private [ossa] @$s{{.*}}12testOptionalyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $OptionalError
// CHECK:         throw {{%.*}}
func testOptional() {
  genericAsync { () async throws(OptionalError) -> Int in
    throw OptionalError(code: nil)
  }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}8testBoolyyF{{.*}}fU_ : $@convention(thin) @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $BoolError
// CHECK:         throw {{%.*}}
func testBool() {
  generic { () throws(BoolError) -> Int in
    throw BoolError(flag: true)
  }
}

func work() async throws(RequestError) -> Int {
  throw RequestError(code: 1, message: "m")
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}11testRequestyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:       bb{{[0-9]+}}([[ERROR:%.*]] : @owned $RequestError):
// CHECK:         [[BOX:%.*]] = alloc_existential_box $any Error, $RequestError
// CHECK:         [[ADDR:%.*]] = project_existential_box $RequestError in [[BOX]]
// CHECK:         store [[ERROR]] to [init] [[ADDR]]
// CHECK:         throw {{%.*}}
func testRequest() {
  genericAsync { () async throws(RequestError) -> Int in try await work() }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}8testEnumyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $EnumError
// CHECK:         throw {{%.*}}
func testEnum() {
  genericAsync { () async throws(EnumError) -> Int in throw .c }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}9testClassyyF{{.*}}fU_ : $@convention(thin) @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $ClassError
// CHECK:         throw {{%.*}}
func testClass() {
  generic { () throws(ClassError) -> Int in throw ClassError() }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}8testTaskyyYaF{{.*}}fU_ : $@convention(thin) {{.*}}@async @substituted <τ_0_0> ({{.*}}) -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $StringError
// CHECK:         throw {{%.*}}
func testTask() async {
  let task = Task { @Sendable () async throws(StringError) -> Int in
    throw StringError(message: "t")
  }
  _ = await task.result
}

// Isolated and `rethrows` parameters take the same path.

func genericNonsending<T>(
  _ op: nonisolated(nonsending) () async throws -> T
) async {}
@MainActor func genericMainActor<T>(
  _ op: @MainActor () async throws -> T
) async {}
func genericRethrows<T>(_ op: () throws -> T) rethrows {}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}14testNonsendingyyYaF :
// CHECK-NOT:     thunk
// CHECK:         function_ref @$s{{.*}}14testNonsendingyyYaF{{.*}}fU_ : $@convention(thin) @caller_isolated @async @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> (@out τ_0_0, @error any Error) for <Int>
// CHECK-NOT:     thunk
// CHECK:       } // end sil function '$s{{.*}}14testNonsendingyyYaF'

// CHECK-LABEL: sil private [ossa] @$s{{.*}}14testNonsendingyyYaF{{.*}}fU_ : $@convention(thin) @caller_isolated @async @substituted <τ_0_0> (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor) -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $StringError
// CHECK:         throw {{%.*}}
func testNonsending() async {
  await genericNonsending { () async throws(StringError) -> Int in
    throw StringError(message: "n")
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}13testMainActoryyYaF :
// CHECK-NOT:     thunk
// CHECK:         function_ref @$s{{.*}}13testMainActoryyYaF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int>
// CHECK-NOT:     thunk
// CHECK:       } // end sil function '$s{{.*}}13testMainActoryyYaF'

// CHECK-LABEL: sil private [ossa] @$s{{.*}}13testMainActoryyYaF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $StringError
// CHECK:         throw {{%.*}}
@MainActor func testMainActor() async {
  await genericMainActor { () async throws(StringError) -> Int in
    throw StringError(message: "m")
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s{{.*}}12testRethrowsyyF :
// CHECK-NOT:     thunk
// CHECK:         function_ref @$s{{.*}}12testRethrowsyyF{{.*}}fU_ : $@convention(thin) @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int>
// CHECK-NOT:     thunk
// CHECK:       } // end sil function '$s{{.*}}12testRethrowsyyF'

// CHECK-LABEL: sil private [ossa] @$s{{.*}}12testRethrowsyyF{{.*}}fU_ : $@convention(thin) @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <Int> {
// CHECK:         alloc_existential_box $any Error, $StringError
// CHECK:         throw {{%.*}}
func testRethrows() {
  try? genericRethrows { () throws(StringError) -> Int in
    throw StringError(message: "r")
  }
}

// In a generic context, the closure is lowered without a substituted function
// type, and its error type is erased in the same way.

// CHECK-LABEL: sil private [ossa] @$s{{.*}}13testArchetype{{.*}}fU_ : $@convention(thin) <F where F : Error> (@in_guaranteed F) -> (@out Int, @error any Error) {
// CHECK:         alloc_existential_box $any Error, $F
// CHECK:         throw {{%.*}}
func testArchetype<F: Error>(_ f: F) {
  generic { () throws(F) -> Int in throw f }
}

// A generic typed throws parameter keeps the closure's concrete error type as
// an indirect typed error; it must not be erased to `any Error`.

func genericTyped<T, E: Error>(_ op: () throws(E) -> T) {}
func genericTypedAsync<T, E: Error>(_ op: () async throws(E) -> T) {}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}16testGenericTypedyyF{{.*}}fU_ : $@convention(thin) @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <StringError, Int> {
// CHECK:       bb0({{%.*}} : $*Int, [[ERROR_OUT:%.*]] : $*StringError):
// CHECK-NOT:     alloc_existential_box
// CHECK:         copy_addr [take] {{%.*}} to [init] [[ERROR_OUT]]
// CHECK-NOT:     alloc_existential_box
// CHECK:         throw_addr
// CHECK:       } // end sil function '$s{{.*}}16testGenericTypedyyF{{.*}}fU_'
func testGenericTyped() {
  genericTyped { () throws(StringError) -> Int in
    throw StringError(message: "g")
  }
}

// CHECK-LABEL: sil private [ossa] @$s{{.*}}21testGenericTypedAsyncyyF{{.*}}fU_ : $@convention(thin) @async @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <StringError, Int> {
// CHECK:       bb0({{%.*}} : $*Int, [[ERROR_OUT:%.*]] : $*StringError):
// CHECK-NOT:     alloc_existential_box
// CHECK:         copy_addr [take] {{%.*}} to [init] [[ERROR_OUT]]
// CHECK-NOT:     alloc_existential_box
// CHECK:         throw_addr
// CHECK:       } // end sil function '$s{{.*}}21testGenericTypedAsyncyyF{{.*}}fU_'
func testGenericTypedAsync() {
  genericTypedAsync { () async throws(StringError) -> Int in
    throw StringError(message: "a")
  }
}
