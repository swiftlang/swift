// RUN: %target-swift-frontend -swift-version 6 -emit-silgen -default-isolation MainActor %s | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -emit-sil -default-isolation MainActor %s -verify

// READ THIS! This test is meant to FileCheck the specific isolation when
// `-default-isolation` is set to `MainActor`. Please do not put other types
// of tests in here.

class Klass {
  // Implicit deinit
  // CHECK: // Klass.deinit
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor5KlassCfd : $@convention(method) (@guaranteed Klass) -> @owned Builtin.NativeObject {

  // Implicit deallocating deinit
  // CHECK: // Klass.__deallocating_deinit
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor5KlassCfD : $@convention(method) (@owned Klass) -> () {
  // CHECK: swift_task_deinitOnExecutor

  // Implicit allocating init
  // CHECK: // Klass.__allocating_init()
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [exact_self_class] [ossa] @$s16assume_mainactor5KlassCACycfC : $@convention(method) (@thick Klass.Type) -> @owned Klass {

  // Implicit designated init
  // CHECK: // Klass.init()
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor5KlassCACycfc : $@convention(method) (@owned Klass) -> @owned Klass {
}

struct StructContainingKlass {
  // CHECK: // variable initialization expression of StructContainingKlass.k
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor21StructContainingKlassV1kAA0E0Cvpfi : $@convention(thin) () -> @owned Klass {

  // CHECK: // StructContainingKlass.k.getter
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor21StructContainingKlassV1kAA0E0Cvg : $@convention(method) (@guaranteed StructContainingKlass) -> @owned Klass {

  // CHECK: // StructContainingKlass.k.setter
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor21StructContainingKlassV1kAA0E0Cvs : $@convention(method) (@owned Klass, @inout StructContainingKlass) -> () {
  var k = Klass()

  // CHECK: // StructContainingKlass.init()
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor21StructContainingKlassVACycfC : $@convention(method) (@thin StructContainingKlass.Type) -> @owned StructContainingKlass {
}

// TODO: Allow for nonisolated to be applied to structs.
struct NonIsolatedStructContainingKlass {
  // CHECK: // variable initialization expression of NonIsolatedStructContainingKlass.k
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor32NonIsolatedStructContainingKlassV1kAA0G0Cvpfi : $@convention(thin) () -> @owned Klass {

  // CHECK: // NonIsolatedStructContainingKlass.k.getter
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor32NonIsolatedStructContainingKlassV1kAA0G0Cvg : $@convention(method) (@guaranteed NonIsolatedStructContainingKlass) -> @owned Klass {

  // CHECK: // NonIsolatedStructContainingKlass.k.setter
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor32NonIsolatedStructContainingKlassV1kAA0G0Cvs : $@convention(method) (@owned Klass, @inout NonIsolatedStructContainingKlass) -> () {
  var k = Klass()

  // CHECK: // NonIsolatedStructContainingKlass.init()
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor32NonIsolatedStructContainingKlassVACycfC : $@convention(method) (@thin NonIsolatedStructContainingKlass.Type) -> @owned NonIsolatedStructContainingKlass {
}

struct NonCopyableStruct: ~Copyable {
  var x: Int
  var y: Int

  // CHECK: NonCopyableStruct.deinit
  // CHECK-NEXT: Isolation: nonisolated
  deinit {
  }
}

@globalActor
actor CustomActor {
  static nonisolated let shared = CustomActor()
}

// CHECK: // unspecifiedAsync<A>(_:)
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor16unspecifiedAsyncyyxYalF : $@convention(thin) @async <T> (@in_guaranteed T) -> () {
func unspecifiedAsync<T>(_ t: T) async {}

// CHECK: // nonisolatedAsync<A>(_:)
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor16nonisolatedAsyncyyxYalF : $@convention(thin) @async <T> (@in_guaranteed T) -> () {
nonisolated func nonisolatedAsync<T>(_ t: T) async {}

// CHECK: // mainActorAsync<A>(_:)
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor14mainActorAsyncyyxYalF : $@convention(thin) @async <T> (@in_guaranteed T) -> () {
@MainActor func mainActorAsync<T>(_ t: T) async {}

// CHECK: // customActorAsync<A>(_:)
// CHECK-NEXT: // Isolation: global_actor. type: CustomActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor16customActorAsyncyyxYalF : $@convention(thin) @async <T> (@in_guaranteed T) -> () {
@CustomActor func customActorAsync<T>(_ t: T) async {}


@CustomActor
struct CustomActorStruct {
  // Variable expression is custom actor... but since we have a nonisolated
  // init, we should be fine?
  //
  // CHECK: // variable initialization expression of CustomActorStruct.k
  // CHECK-NEXT: // Isolation: global_actor. type: CustomActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor17CustomActorStructV1kAA5KlassCvpfi : $@convention(thin) () -> @owned Klass {

  // CHECK: // CustomActorStruct.k.getter
  // CHECK-NEXT: // Isolation: global_actor. type: CustomActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor17CustomActorStructV1kAA5KlassCvg : $@convention(method) (@guaranteed CustomActorStruct) -> @owned Klass {

  // CHECK: // CustomActorStruct.k.setter
  // CHECK-NEXT: // Isolation: global_actor. type: CustomActor
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor17CustomActorStructV1kAA5KlassCvs : $@convention(method) (@owned Klass, @inout CustomActorStruct) -> () {
  var k = Klass()
}

// CHECK: // unspecifiedFunctionTest()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor23unspecifiedFunctionTestyyYaF : $@convention(thin) @async () -> () {
func unspecifiedFunctionTest() async {
  let k = Klass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)
}

// CHECK: // unspecifiedFunctionTest2()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor24unspecifiedFunctionTest2yyYaF : $@convention(thin) @async () -> () {
func unspecifiedFunctionTest2() async {
  let k = StructContainingKlass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)

  await unspecifiedAsync(k.k)
  await nonisolatedAsync(k.k)
  await mainActorAsync(k.k)
}

// CHECK: // unspecifiedFunctionTest3()
// CHECK-NEXT: // Isolation: global_actor. type: MainActor
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor24unspecifiedFunctionTest3yyYaF : $@convention(thin) @async () -> () {
func unspecifiedFunctionTest3() async {
  let k = NonIsolatedStructContainingKlass()
  await unspecifiedAsync(k)
  await nonisolatedAsync(k)
  await mainActorAsync(k)

  await unspecifiedAsync(k.k)
  await nonisolatedAsync(k.k)
  await mainActorAsync(k.k)
}

// CHECK: // nonisolatedFunctionTest()
// CHECK-NEXT: // Isolation: nonisolated
// CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor23nonisolatedFunctionTestyyYaF : $@convention(thin) @async () -> () {
nonisolated func nonisolatedFunctionTest() async {
  let k = NonIsolatedStructContainingKlass()
  await unspecifiedAsync(k.k)
  await nonisolatedAsync(k.k)
  await mainActorAsync(k.k)
}

actor MyActor {
  // CHECK: // variable initialization expression of MyActor.k
  // CHECK-NEXT: // Isolation: unspecified
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor7MyActorC1kAA5KlassCvpfi : $@convention(thin) () -> @owned Klass {

  // CHECK: // MyActor.k.getter
  // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor7MyActorC1kAA5KlassCvg : $@convention(method) (@sil_isolated @guaranteed MyActor) -> @owned Klass {

  // CHECK: // MyActor.k.setter
  // CHECK-NEXT: // Isolation: actor_instance. name: 'self'
  // CHECK-NEXT: sil hidden [transparent] [ossa] @$s16assume_mainactor7MyActorC1kAA5KlassCvs : $@convention(method) (@owned Klass, @sil_isolated @guaranteed MyActor) -> () {
  var k = Klass()

  // CHECK: // static MyActor.f()
  // CHECK-NEXT: // Isolation: unspecified
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor7MyActorC1fyyFZ : $@convention(method) (@thick MyActor.Type) -> ()
  static func f() {}

  struct Nested {
    // CHECK: // MyActor.Nested.f()
    // CHECK-NEXT: // Isolation: unspecified
    // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor7MyActorC6NestedV1fyyF : $@convention(method) (MyActor.Nested) -> ()
    func f() {}
  }

  // Implicit deinit
  // CHECK: // MyActor.deinit
  // CHECK-NEXT: // Isolation: unspecified
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor7MyActorCfd : $@convention(method) (@guaranteed MyActor) -> @owned Builtin.NativeObject {

  // Non-async init should be nonisolated
  // CHECK: // MyActor.init()
  // CHECK-NEXT: // Isolation: nonisolated
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor7MyActorCACycfc : $@convention(method) (@owned MyActor) -> @owned MyActor {
}

actor MyActor2 {
  // CHECK: // MyActor2.init()
  // CHECK-NEXT: // Isolation: global_actor. type: MainActor
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor8MyActor2CACycfc : $@convention(method) (@owned MyActor2) -> @owned MyActor2 {
  @MainActor
  init() {}

  // CHECK: // MyActor2.init(x:)
  // CHECK-NEXT: // Isolation: global_actor. type: CustomActor
  // CHECK-NEXT: sil hidden [ossa] @$s16assume_mainactor8MyActor2C1xACyt_tcfc : $@convention(method) (@owned MyActor2) -> @owned MyActor2 {
  @CustomActor
  init(x: ()) {}
}

@CustomActor func validateThatPrintIsStillNonIsolated() {
  // Since we are in a CustomActor, we can only call this if print is
  // NonIsolated and not if print was inferred to be main actor.
  print("123")
}
