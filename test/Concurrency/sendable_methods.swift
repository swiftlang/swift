// RUN: %target-typecheck-verify-swift -enable-experimental-feature InferSendableFromCaptures -disable-availability-checking
// RUN: %target-swift-emit-silgen %s -verify -enable-experimental-feature InferSendableFromCaptures -disable-availability-checking -module-name sendable_methods | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: asserts

func outer() {
    @Sendable func sendable() {}

    func nonSendable() {}

    let _ : @Sendable () -> Void = sendable
    let _ : @Sendable () -> Void = nonSendable // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}
}


final class C : Sendable {
  func f() {}
}

struct S : Sendable {
  func f(){}
}

enum E : Sendable {
  case a, b, c
  
  func f() {}
}

protocol P : Sendable {
  init()
}

final class InferredSendableC: P {
  func f() { }
}

struct InferredSendableS: P {
  func f() { }
}

enum InferredSendableE: P {
  case a, b
  
  func f() { }
}

final class GenericC<T>: P {
  func f() { }
}

struct GenericS<T> : P {
  init(_: T) { }

  init() { }

  func f() { }

  func g() async { }
}

class NonSendable {
  func f() {}
}

func g<T>(_ f: @escaping @Sendable (T) -> (@Sendable () -> Void)) where T: P {
  Task {
    let instance = T()
    f(instance)()
  }
}
// Unapplied Func Parameters
g(GenericS<NonSendable>.f)  // ok because unapplied references don't capture state
g(GenericC<NonSendable>.f)
g(InferredSendableS.f)
g(InferredSendableC.f)
g(InferredSendableE.f)
g(GenericS<Int>.f)
g(GenericC<Int>.f)

struct GenericQ<T> {

  func f() { }
}

func g2<T>(_ f: @Sendable (T) -> (@Sendable () -> Void)) { }
g2(GenericQ<NonSendable>.f) // ok because unapplied references don't capture state


// Conditional Conformances
//extension GenericS : Sendable where T : Sendable { }
//extension GenericC : Sendable where T : Sendable { }

extension E {
  init(){
    self = .a
  }
}

extension InferredSendableE {
  init(){
    self = .a
  }
}

// Partial Apply Parameters
func h(_ f: (@Sendable () -> Void)) {  }
h(GenericQ<NonSendable>().f) // ok
h(GenericS(NonSendable()).f) // ok
h(GenericS<Int>().f)
h(GenericS(1).f)
h(NonSendable().f) // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}

func executeAsTask (_ f: @escaping  @Sendable () -> Void) {
  Task {
    f()
  }
}
executeAsTask(S().f)
executeAsTask(C().f)
executeAsTask(E().f)
executeAsTask(NonSendable().f) // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}

do {
  let f = S.f
  let _ : @Sendable () -> () = f(S())
}

// Declarations
let us:  @Sendable (GenericS<Int>) -> (@Sendable () -> Void) = GenericS<Int>.f
let uc:  @Sendable (GenericC<Int>) -> (@Sendable () -> Void) = GenericC<Int>.f

let unappliedStruct:  @Sendable (S) -> (@Sendable () -> Void) = S.f
let unappliedClass:  @Sendable (C) -> (@Sendable () -> Void) = C.f
let unappliedEnum:  @Sendable (E) -> (@Sendable () -> Void) = E.f

var partialStruct : @Sendable () -> Void = S().f
var partialClass : @Sendable () -> Void = C().f
var partialEnum : @Sendable () -> Void = E().f

// Reassign
partialClass = NonSendable().f // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}
partialStruct = NonSendable().f // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}
partialEnum = NonSendable().f // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}

// Static Functions 
struct World {
  static func greet () { print("hello") }
}

let helloworld:  @Sendable () -> Void = World.greet

class NonSendableC {
    var x: Int = 0

    @Sendable func inc() { // expected-warning {{instance methods of non-Sendable types cannot be marked as '@Sendable'; this is an error in Swift 6}}
        x += 1
    }
}

func doWork() -> Int {
  Int.random(in: 1..<42)
}

// unapplied global func call
let work: @Sendable () -> Int = doWork
Task<Int, Never>.detached(priority: nil, operation: doWork)
Task<Int, Never>.detached(priority: nil, operation: work)

// generic argument for `T` should be inferred as `@escaping @Sendable`
func generic2<T>(_ f: T) { }

// CHECK-LABEL: sil hidden [ossa] @$s16sendable_methods12testGeneric2yyF : $@convention(thin) () -> ()
// CHECK: [[F:%.*]] = alloc_stack $@Sendable @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <GenericS<Int>, @Sendable () -> ()>
// CHECK: [[GENERIC_2_FN:%.*]] = function_ref @$s16sendable_methods8generic2yyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: {{.*}} = apply [[GENERIC_2_FN]]<@Sendable (GenericS<Int>) -> @Sendable () -> ()>([[F]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
func testGeneric2() {
  generic2(GenericS<Int>.f)
}

actor TestActor {}

@globalActor
struct SomeGlobalActor {
  static var shared: TestActor { TestActor() }
}

@SomeGlobalActor
let globalValue: NonSendable = NonSendable()


@SomeGlobalActor
// CHECK-LABEL: sil hidden [ossa] @$s16sendable_methods8generic3yyxYalF : $@convention(thin) @async <T> (@in_guaranteed T) -> ()
//
// CHECK: [[F2:%.*]] = alloc_stack $@Sendable @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <GenericS<String>, @Sendable () -> ()>
// CHECK: [[GENERIC_3_FN:%.*]] = function_ref @$s16sendable_methods8generic3yyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: {{.*}} = apply [[GENERIC_3_FN]]<@Sendable (GenericS<String>) -> @Sendable () -> ()>([[F2]]) : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
//
// CHECK: [[F3:%.*]] = alloc_stack $@Sendable @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <GenericS<NonSendable>, @Sendable () -> ()>
// CHECK: [[GENERIC_3_FN:%.*]] = function_ref @$s16sendable_methods8generic3yyxYalF : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: {{.*}} = apply [[GENERIC_3_FN]]<@Sendable (GenericS<NonSendable>) -> @Sendable () -> ()>([[F3]]) : $@convention(thin) @async <τ_0_0> (@in_guaranteed τ_0_0) -> ()
func generic3<T>(_ x: T) async {

  await generic3(GenericS<String>.f)

  await generic3(GenericS<NonSendable>.f)
}
