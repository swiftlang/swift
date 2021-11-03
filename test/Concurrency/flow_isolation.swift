// RUN: %target-swift-frontend -parse-as-library -O -emit-sil -verify %s

func randomBool() -> Bool { return false }
func logTransaction(_ i: Int) {}

enum Color: Error {
  case red
  case yellow
  case blue
}

func takeNonSendable(_ ns: NonSendableType) {}
func takeSendable(_ s: SendableType) {}

class NonSendableType {
  var x: Int = 0
}

struct SendableType: Sendable {}

struct Money {
  var dollars: Int
  var euros: Int {
    return dollars * 2
  }
}

func takeBob(_ b: Bob) {}

actor Bob {
  var x: Int

  nonisolated func speak() { }
  nonisolated var cherry : String {
        get { "black cherry" }
    }

  init(v0 initial: Int) {
    self.x = 0
    speak()    // expected-note {{after calling instance method 'speak()', only non-isolated properties of 'self' can be accessed from this init}}
    speak()
    self.x = 1 // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
    speak()
  }

  init(v1 _: Void) {
    self.x = 0
    _ = cherry  // expected-note {{after accessing property 'cherry', only non-isolated properties of 'self' can be accessed from this init}}
    self.x = 1  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
  }

  init(v2 callBack: (Bob) -> Void) {
    self.x = 0
    callBack(self)  // expected-note {{after a call involving 'self', only non-isolated properties of 'self' can be accessed from this init}}
    self.x = 1      // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
  }

  init(v3 _: Void) {
    self.x = 1
    takeBob(self)   // expected-note {{after calling global function 'takeBob', only non-isolated properties of 'self' can be accessed from this init}}
    self.x = 1  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
  }



  // ensure noniso does not flow out of a defer's unreachable block
  init(spice doesNotFlow: Int) {
    if true {
      defer {
        if randomBool() {
          speak()
          fatalError("oops")
        }
      }
      self.x = 0
    }
    self.x = 20
  }
}

actor Casey {
  var money: Money

  nonisolated func speak(_ msg: String) { print("Casey: \(msg)") }
  nonisolated func cashUnderMattress() -> Int { return 0 }

  init() {
    money = Money(dollars: 100)
    defer { logTransaction(money.euros) } // expected-warning {{cannot access property 'money' here in non-isolated initializer}}
    self.speak("Yay, I have $\(money.dollars)!") // expected-note {{after calling instance method 'speak', only non-isolated properties of 'self' can be accessed from this init}}
  }

  init(with start: Int) {
    money = Money(dollars: start)

    if (money.dollars < 0) {
      self.speak("Oh no, I'm in debt!") // expected-note 3 {{after calling instance method 'speak', only non-isolated properties of 'self' can be accessed from this init}}
    }
    logTransaction(money.euros) // expected-warning {{cannot access property 'money' here in non-isolated initializer}}

    // expected-warning@+1 2 {{cannot access property 'money' here in non-isolated initializer}}
    money.dollars = money.dollars + 1

    if randomBool() {
      // expected-note@+2 {{after calling instance method 'cashUnderMattress()', only non-isolated properties of 'self' can be accessed from this init}}
      // expected-warning@+1 {{cannot access property 'money' here in non-isolated initializer}}
      money.dollars += cashUnderMattress()
    }
  }
}

actor Demons {
  let ns: NonSendableType

  init(_ x: NonSendableType) {
    self.ns = x
  }

  deinit {
    let _ = self.ns // expected-warning {{cannot access property 'ns' with a non-sendable type 'NonSendableType' from non-isolated deinit}}
  }
}

func pass<T>(_ t: T) {}

actor ExampleFromProposal {
  let immutableSendable = SendableType()
  var mutableSendable = SendableType()
  let nonSendable = NonSendableType()

  init() {
    _ = self.immutableSendable  // ok
    _ = self.mutableSendable    // ok
    _ = self.nonSendable        // ok

    f() // expected-note 2 {{after calling instance method 'f()', only non-isolated properties of 'self' can be accessed from this init}}

    _ = self.immutableSendable  // ok
    _ = self.mutableSendable    // expected-warning {{cannot access property 'mutableSendable' here in non-isolated initializer}}
    _ = self.nonSendable        // expected-warning {{cannot access property 'nonSendable' here in non-isolated initializer}}
  }


  deinit {
    _ = self.immutableSendable  // ok
    _ = self.mutableSendable    // ok
    _ = self.nonSendable        // expected-warning {{cannot access property 'nonSendable' with a non-sendable type 'NonSendableType' from non-isolated deinit}}

    f() // expected-note {{after calling instance method 'f()', only non-isolated properties of 'self' can be accessed from a deinit}}

    _ = self.immutableSendable  // ok
    _ = self.mutableSendable    // expected-warning {{cannot access property 'mutableSendable' here in deinitializer}}
    _ = self.nonSendable        // expected-warning {{cannot access property 'nonSendable' with a non-sendable type 'NonSendableType' from non-isolated deinit}}
  }

  nonisolated func f() {}
}

@MainActor
class CheckGAIT1 {
  var silly = 10
  var ns = NonSendableType()

  nonisolated func f() {}

  nonisolated init(_ c: Color) {
    silly += 0
    repeat {
      switch c {
        case .red:
          continue
        case .yellow:
          silly += 1
          continue
        case .blue:
          f() // expected-note {{after calling instance method 'f()', only non-isolated properties of 'self' can be accessed from this init}}
      }
      break
    } while true
    silly += 2 // expected-warning {{cannot access property 'silly' here in non-isolated initializer}}
  }

  deinit {
    _ = ns // expected-warning {{cannot access property 'ns' with a non-sendable type 'NonSendableType' from non-isolated deinit}}
    f()     // expected-note {{after calling instance method 'f()', only non-isolated properties of 'self' can be accessed from a deinit}}
    _ = silly // expected-warning {{cannot access property 'silly' here in deinitializer}}

  }
}

actor ControlFlowTester {
  let ns: NonSendableType = NonSendableType()
  let s: SendableType = SendableType()
  var count: Int = 0

  nonisolated func noniso() {}

  // FIXME: since we're allowing let-bound sendable property reads,
  // these error messages need tweaking. We may actually want to say
  // that "cannot access it while self is nonisolated" and say the it became
  // nonisolated from some point. That's probably the most informative explaination.
  init(v1: Void) {
    noniso()                 // expected-note {{after calling instance method 'noniso()', only non-isolated properties of 'self' can be accessed from this init}}
    takeNonSendable(self.ns) // expected-warning {{cannot access property 'ns' here in non-isolated initializer}}
    takeSendable(self.s)
  }

  init(v2 c: Color) throws {
    if c == .red {
      noniso()
      throw c
    } else if c == .blue {
      noniso()
      fatalError("soul")
    }
    count += 1
  }

  init(v3 c: Color) {
    do {
      noniso() // expected-note 2 {{after calling instance method 'noniso()', only non-isolated properties of 'self' can be accessed from this init}}

      throw c

    } catch Color.blue {
      count += 1 // expected-warning {{cannot access property 'count' here in non-isolated initializer}}
    } catch {
      count += 1 // expected-warning {{cannot access property 'count' here in non-isolated initializer}}
    }
  }


  init(v4 c: Color) {
    defer { count += 1 } // expected-warning {{cannot access property 'count' here in non-isolated initializer}}
    noniso() // expected-note 1 {{after calling instance method 'noniso()', only non-isolated properties of 'self' can be accessed from this init}}
  }
}
