// RUN: %target-parse-verify-swift

protocol P { }
@objc protocol OP { }

func f1<T: P>(t: T) { } // expected-note 2{{in call to function 'f1'}}
func f2<T: OP>(t: T) { }
func f3<T>(t: T) { }

func testPassExistential(p: P, op: OP, opp: protocol<P, OP>, any: Any) {
  f1(p) // expected-error{{generic parameter 'T' cannot be bound to non-@objc protocol type 'P'}}
  f3(p)

  f2(op)
  f3(op)

  f1(opp) // expected-error{{generic parameter 'T' cannot be bound to non-@objc protocol type 'protocol<OP, P>'}}
  f2(opp)
  f3(opp)

  f3(any)
}

// rdar://problem/21087341
protocol Mine {}
class M1: Mine {}
class M2: Mine {}
extension CollectionType where Generator.Element : Mine {
    final func takeAll() {}
}

func foo() {
  let allMine: [Mine] = [M1(), M2(), M1()]
  allMine.takeAll() // expected-error{{protocol type 'Mine' cannot be used as a generic argument conforming to non-@objc protocol 'Mine'}}
}
