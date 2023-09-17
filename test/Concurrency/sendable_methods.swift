// RUN: %target-typecheck-verify-swift -enable-experimental-feature InferSendableMethods
// REQUIRES: concurrency

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

func g<T>(_ f: @escaping @Sendable (T) -> (@Sendable () -> Void)) where T: P {
  Task {
    let instance = T()
    f(instance)()
  }
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

struct GenericS<T>: P {
  func f() { }
}

final class GenericC<T>: P {
  func f() { }
}

// Conditional Conformances
extension GenericS : Sendable where T : Sendable { }
extension GenericC : Sendable where T : Sendable { }

class NonSendable {
  func f() {}
}

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

// Unapplied Func Parameter
g(InferredSendableS.f)
g(InferredSendableC.f)
g(InferredSendableE.f)

g(GenericS<Int>.f)
g(GenericC<Int>.f)

g(GenericS<NonSendable>.f) // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races
g(GenericC<NonSendable>.f) // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races

func executeAsTask (_ f: @escaping  @Sendable () -> Void) {
  Task {
    f()
  }
}

// Partial Apply Parameter
executeAsTask(C().f)
executeAsTask(S().f)
executeAsTask(E().f)
executeAsTask(NonSendable().f) // expected-warning{{converting non-sendable function value to '@Sendable () -> Void' may introduce data races}}

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

    // TO-DO: Invalidate Sendable annotation in follow-up PR
    @Sendable func inc() {
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
