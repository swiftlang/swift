// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

protocol P: Actor {
  func f()
  var prop: Int { get set } // expected-note 2 {{mutation of this property is only permitted within the actor}}
}

actor A: P {
  var prop: Int = 0 // expected-note 2 {{mutation of this property is only permitted within the actor}}
  func f() {}
}

func from_isolated_existential1(_ x: isolated any P) {
  x.f()
  x.prop += 1
  x.prop = 100
}

func from_isolated_existential2(_ x: isolated any P) async {
  x.f()
  x.prop += 1
  x.prop = 100
}

func from_nonisolated(_ x: any P) async {
  await x.f()
  x.prop += 1 // expected-error {{actor-isolated property 'prop' can not be mutated from a nonisolated context}}
  // expected-note@-1 {{consider declaring an isolated method on 'P' to perform the mutation}}
  x.prop = 100 // expected-error {{actor-isolated property 'prop' can not be mutated from a nonisolated context}}
  // expected-note@-1 {{consider declaring an isolated method on 'P' to perform the mutation}}
}

func from_concrete(_ x: A) async {
  x.prop += 1 // expected-error {{actor-isolated property 'prop' can not be mutated from a nonisolated context}}
  // expected-note@-1 {{consider declaring an isolated method on 'A' to perform the mutation}}
  x.prop = 100 // expected-error {{actor-isolated property 'prop' can not be mutated from a nonisolated context}}
  // expected-note@-1 {{consider declaring an isolated method on 'A' to perform the mutation}}
}

func from_isolated_concrete(_ x: isolated A) async {
  x.prop += 1
  x.prop = 100
}


// from https://github.com/apple/swift/issues/59573
actor Act {
    var i = 0 // expected-note {{mutation of this property is only permitted within the actor}}
}
nonisolated let act = Act()

func bad() async {
    // expected-warning@+3 {{no 'async' operations occur within 'await' expression}}
    // expected-error@+2 {{actor-isolated property 'i' can not be mutated from a nonisolated context}}
    // expected-note@+1 {{consider declaring an isolated method on 'Act' to perform the mutation}}
    await act.i = 666
}

protocol Proto: Actor {
    var i: Int { get set } // expected-note 2 {{mutation of this property is only permitted within the actor}}
}
extension Act: Proto {}

func good() async {
    // expected-warning@+3 {{no 'async' operations occur within 'await' expression}}
    // expected-error@+2 {{actor-isolated property 'i' can not be mutated from a nonisolated context}}
    // expected-note@+1 {{consider declaring an isolated method on 'Proto' to perform the mutation}}
    await (act as any Proto).i = 42
    let aIndirect: any Proto = act

    // expected-warning@+3 {{no 'async' operations occur within 'await' expression}}
    // expected-error@+2 {{actor-isolated property 'i' can not be mutated from a nonisolated context}}
    // expected-note@+1 {{consider declaring an isolated method on 'Proto' to perform the mutation}}
    await aIndirect.i = 777
}
