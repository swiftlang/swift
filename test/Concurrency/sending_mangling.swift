// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -emit-silgen -swift-version 6 | swift-demangle | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-silgen -swift-version 6 | %FileCheck -check-prefix=SIL %s

// REQUIRES: concurrency
// REQUIRES: asserts

class NonSendableKlass {}

struct S<T> {
  var count: Int { 0 }
}

// CHECK: sil hidden [ossa] @sending_mangling.testRemoveFunctionArg(__owned sending_mangling.NonSendableKlass) -> () : $@convention(thin) (@sil_sending @owned NonSendableKlass) -> () {
func testRemoveFunctionArg(_ x: sending NonSendableKlass) {}

// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionArgSubTypeArg(__owned sending_mangling.S<(sending __owned sending_mangling.NonSendableKlass) -> ()>) -> () : $@convention(thin) (@sil_sending S<(sending NonSendableKlass) -> ()>) -> () {
func testNoRemoveFunctionArgSubTypeArg(_ x: sending S<(sending NonSendableKlass) -> ()>) {}

// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionArgSubTypeReturn(__owned sending_mangling.S<() -> sending sending_mangling.NonSendableKlass>) -> () : $@convention(thin) (@sil_sending S<() -> sending NonSendableKlass>) -> () {
func testNoRemoveFunctionArgSubTypeReturn(_ x: sending S<() -> sending NonSendableKlass>) {}

// CHECK: sil hidden [ossa] @sending_mangling.testRemoveFunctionResult() -> sending_mangling.NonSendableKlass : $@convention(thin) () -> @sil_sending @owned NonSendableKlass {
func testRemoveFunctionResult() -> sending NonSendableKlass {
}

// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionResultSubTypeArg() -> sending_mangling.S<(sending __owned sending_mangling.NonSendableKlass) -> ()> : $@convention(thin) () -> @sil_sending S<(sending NonSendableKlass) -> ()> {
func testNoRemoveFunctionResultSubTypeArg() -> sending S<(sending NonSendableKlass) -> ()> { fatalError() }

// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionResultSubTypeResult() -> sending_mangling.S<() -> sending sending_mangling.NonSendableKlass> : $@convention(thin) () -> @sil_sending S<() -> sending NonSendableKlass> {
func testNoRemoveFunctionResultSubTypeResult() -> sending S<() -> sending NonSendableKlass> { fatalError() }

// We do not remove this since the sending is in the subtype of the result.
// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionResultImmediateTypedFunctionWithArg() -> (sending __owned sending_mangling.NonSendableKlass) -> () : $@convention(thin) () -> @owned @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> () {
func testNoRemoveFunctionResultImmediateTypedFunctionWithArg() -> ((sending NonSendableKlass) -> ()) { fatalError() }

// CHECK: sil hidden [ossa] @sending_mangling.testNoRemoveFunctionResultImmediateTypedFunctionWithResult() -> () -> sending sending_mangling.NonSendableKlass : $@convention(thin) () -> @owned @callee_guaranteed () -> @sil_sending @owned NonSendableKlass {
func testNoRemoveFunctionResultImmediateTypedFunctionWithResult() -> (() -> sending NonSendableKlass) { fatalError() }

struct MethodTest {
  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.init(__owned sending_mangling.NonSendableKlass) -> sending_mangling.MethodTest : $@convention(method) (@sil_sending @owned NonSendableKlass, @thin MethodTest.Type) -> MethodTest {
  init(_ x: sending NonSendableKlass) {}

  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testMethodRemoveFunctionArg(__owned sending_mangling.NonSendableKlass) -> () : $@convention(method) (@sil_sending @owned NonSendableKlass, MethodTest) -> () {
  func testMethodRemoveFunctionArg(_ x: sending NonSendableKlass) {}
  
  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionArgSubTypeArg(__owned sending_mangling.S<(sending __owned sending_mangling.NonSendableKlass) -> ()>) -> () : $@convention(method) (@sil_sending S<(sending NonSendableKlass) -> ()>, MethodTest) -> () {
  func testNoRemoveFunctionArgSubTypeArg(_ x: sending S<(sending NonSendableKlass) -> ()>) {}
  
  // DEMANGLE sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionArgSubTypeReturn(__owned sending_mangling.S<() -> sending sending_mangling.NonSendableKlass>) -> () : $@convention(method) (@sil_sending S<() -> sending NonSendableKlass>, MethodTest) -> () {
  func testNoRemoveFunctionArgSubTypeReturn(_ x: sending S<() -> sending NonSendableKlass>) {}

  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testMethodRemoveFunctionResult() -> sending_mangling.NonSendableKlass : $@convention(method) (MethodTest) -> @sil_sending @owned NonSendableKlass {
  func testMethodRemoveFunctionResult() -> sending NonSendableKlass {
  }

  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionResultSubTypeArg() -> sending_mangling.S<(sending __owned sending_mangling.NonSendableKlass) -> ()> : $@convention(method) (MethodTest) -> @sil_sending S<(sending NonSendableKlass) -> ()> {
  func testNoRemoveFunctionResultSubTypeArg() -> sending S<(sending NonSendableKlass) -> ()> { fatalError() }
  
  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionResultSubTypeResult() -> sending_mangling.S<() -> sending sending_mangling.NonSendableKlass> : $@convention(method) (MethodTest) -> @sil_sending S<() -> sending NonSendableKlass> {
  func testNoRemoveFunctionResultSubTypeResult() -> sending S<() -> sending NonSendableKlass> { fatalError() }

  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionResultImmediateTypedFunctionWithArg() -> (sending __owned sending_mangling.NonSendableKlass) -> () : $@convention(method) (MethodTest) -> @owned @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> () {
  func testNoRemoveFunctionResultImmediateTypedFunctionWithArg() -> ((sending NonSendableKlass) -> ()) { fatalError() }

  // CHECK: sil hidden [ossa] @sending_mangling.MethodTest.testNoRemoveFunctionResultImmediateTypedFunctionWithResult() -> () -> sending sending_mangling.NonSendableKlass : $@convention(method) (MethodTest) -> @owned @callee_guaranteed () -> @sil_sending @owned NonSendableKlass {
  func testNoRemoveFunctionResultImmediateTypedFunctionWithResult() -> (() -> sending NonSendableKlass) { fatalError() }
}

protocol SendingProtocol {
  func sendingArg(_ x: sending NonSendableKlass)
  func sendingResult() -> sending NonSendableKlass
  func sendingArgWithFunctionSendingArg(_ x: sending (sending NonSendableKlass) -> ())
  func sendingArgWithFunctionSendingResult() -> sending (sending NonSendableKlass) -> ()
}

extension SendingProtocol {
  // CHECK: sil hidden [ossa] @(extension in sending_mangling):sending_mangling.SendingProtocol.sendingArg(__owned sending_mangling.NonSendableKlass) -> () : $@convention(method) <Self where Self : SendingProtocol> (@sil_sending @owned NonSendableKlass, @in_guaranteed Self) -> () {
  func sendingArg(_ x: sending NonSendableKlass) {}

  // CHECK: sil hidden [ossa] @(extension in sending_mangling):sending_mangling.SendingProtocol.sendingResult() -> sending_mangling.NonSendableKlass : $@convention(method) <Self where Self : SendingProtocol> (@in_guaranteed Self) -> @sil_sending @owned NonSendableKlass {
  func sendingResult() -> sending NonSendableKlass { fatalError() }

  // CHECK: sil hidden [ossa] @(extension in sending_mangling):sending_mangling.SendingProtocol.sendingArgWithFunctionSendingArg((sending __owned sending_mangling.NonSendableKlass) -> ()) -> () : $@convention(method) <Self where Self : SendingProtocol> (@sil_sending @guaranteed @noescape @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> (), @in_guaranteed Self) -> () {
  func sendingArgWithFunctionSendingArg(_ x: sending (sending NonSendableKlass) -> ()) {}

  // CHECK: sil hidden [ossa] @(extension in sending_mangling):sending_mangling.SendingProtocol.sendingArgWithFunctionSendingResult() -> (sending __owned sending_mangling.NonSendableKlass) -> () : $@convention(method) <Self where Self : SendingProtocol> (@in_guaranteed Self) -> @sil_sending @owned @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> () {
  func sendingArgWithFunctionSendingResult() -> sending (sending NonSendableKlass) -> () { fatalError() }

  // CHECK: sil hidden [ossa] @(extension in sending_mangling):sending_mangling.SendingProtocol.sendingArgWithEscapingFunctionSendingArg(__owned (sending __owned sending_mangling.NonSendableKlass) -> ()) -> () : $@convention(method) <Self where Self : SendingProtocol> (@sil_sending @owned @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> (), @in_guaranteed Self) -> () {
  func sendingArgWithEscapingFunctionSendingArg(_ x: sending @escaping (sending NonSendableKlass) -> ()) {}
}

// Make sure we only do not mangle in __shared if we are borrowed by default and
// have sending.
//
// CHECK: sil hidden [ossa] @sending_mangling.sendingArgWithShared(sending_mangling.NonSendableKlass) -> () : $@convention(thin) (@sil_sending @guaranteed NonSendableKlass) -> () {
func sendingArgWithShared(_ x: __shared sending NonSendableKlass) {}

// CHECK: sil hidden [ossa] @sending_mangling.argWithShared(__shared sending_mangling.NonSendableKlass) -> () : $@convention(thin) (@guaranteed NonSendableKlass) -> () {
func argWithShared(_ x: __shared NonSendableKlass) {}

struct ConstructorSharedTest {
  // Inits take their value at +1, so we need to mangle in shared even if we do
  // not mangle in sending itself.
  //
  // CHECK: sil hidden [ossa] @sending_mangling.ConstructorSharedTest.init(__shared sending_mangling.NonSendableKlass) -> sending_mangling.ConstructorSharedTest : $@convention(method) (@sil_sending @guaranteed NonSendableKlass, @thin ConstructorSharedTest.Type) -> ConstructorSharedTest {
  init(_ x: __shared sending NonSendableKlass) {}

  // This is a func which takes its parameter at +0 so we should suppress both.
  //
  // CHECK: sil hidden [ossa] @sending_mangling.ConstructorSharedTest.functionSuppressed(sending_mangling.NonSendableKlass) -> () : $@convention(method) (@sil_sending @guaranteed NonSendableKlass, ConstructorSharedTest) -> () {
  func functionSuppressed(_ x: __shared sending NonSendableKlass) {}
}

// Make sure that we produce the appropriate reabstraction thunk.
func reabstractionThunkTest_takeSendingReturnSending<T>(
  _ x: sending T) -> sending T { fatalError() }
func reabstractionThunkTest_reabstractionThunkGenerator<T>(
  _ x: sending T,
  _ f: (sending T) -> T) {}

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @reabstraction thunk helper from @escaping @callee_guaranteed (@in sending sending_mangling.NonSendableKlass) -> sending (@out sending_mangling.NonSendableKlass) to @escaping @callee_guaranteed (@owned sending sending_mangling.NonSendableKlass) -> sending (@owned sending_mangling.NonSendableKlass) : $@convention(thin) (@sil_sending @owned NonSendableKlass, @guaranteed @callee_guaranteed (@sil_sending @in NonSendableKlass) -> @sil_sending @out NonSendableKlass) -> @sil_sending @owned NonSendableKlass {
// SIL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s16sending_mangling16NonSendableKlassCACIegTiTr_A2CIegTxTo_TR : $@convention(thin) (@sil_sending @owned NonSendableKlass, @guaranteed @callee_guaranteed (@sil_sending @in NonSendableKlass) -> @sil_sending @out NonSendableKlass) -> @sil_sending @owned NonSendableKlass {

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @reabstraction thunk helper from @callee_guaranteed (@owned sending sending_mangling.NonSendableKlass) -> (@owned sending_mangling.NonSendableKlass) to @escaping @callee_guaranteed (@in sending sending_mangling.NonSendableKlass) -> (@out sending_mangling.NonSendableKlass) : $@convention(thin) (@sil_sending @in NonSendableKlass, @guaranteed @noescape @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> @owned NonSendableKlass) -> @out NonSendableKlass {
// SIL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s16sending_mangling16NonSendableKlassCACIgxTo_A2CIegiTr_TR : $@convention(thin) (@sil_sending @in NonSendableKlass, @guaranteed @noescape @callee_guaranteed (@sil_sending @owned NonSendableKlass) -> @owned NonSendableKlass) -> @out NonSendableKlass {
func reabstractionThunkTest() {
  reabstractionThunkTest_reabstractionThunkGenerator(
    NonSendableKlass(),
    reabstractionThunkTest_takeSendingReturnSending)
}
