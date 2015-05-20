// RUN: %target-parse-verify-swift

struct S {
  var x: Int = 0
  let y: Int = 0 // expected-note 4 {{change 'let' to 'var' to make it mutable}}

  init() {}
}

struct T {
  var mutS: S? = nil
  let immS: S? = nil // expected-note 6 {{change 'let' to 'var' to make it mutable}}

  init() {}
}

var mutT: T?
let immT: T? = nil

let mutTPayload = mutT!

mutT! = T()
mutT!.mutS = S()
mutT!.mutS! = S()
mutT!.mutS!.x = 0
mutT!.mutS!.y = 0 // expected-error{{cannot assign}}
mutT!.immS = S() // expected-error{{cannot assign}}
mutT!.immS! = S() // expected-error{{cannot assign}}
mutT!.immS!.x = 0 // expected-error{{cannot assign}}
mutT!.immS!.y = 0 // expected-error{{cannot assign}}

immT! = T() // expected-error{{cannot assign}}
immT!.mutS = S() // expected-error{{cannot assign}}
immT!.mutS! = S() // expected-error{{cannot assign}}
immT!.mutS!.x = 0 // expected-error{{cannot assign}}
immT!.mutS!.y = 0 // expected-error{{cannot assign}}
immT!.immS = S() // expected-error{{cannot assign}}
immT!.immS! = S() // expected-error{{cannot assign}}
immT!.immS!.x = 0 // expected-error{{cannot assign}}
immT!.immS!.y = 0 // expected-error{{cannot assign}}

var mutIUO: T! = nil
let immIUO: T! = nil

mutIUO!.mutS = S()
mutIUO!.immS = S() // expected-error{{cannot assign}}
immIUO!.mutS = S() // expected-error{{cannot assign}}
immIUO!.immS = S() // expected-error{{cannot assign}}

mutIUO.mutS = S()
mutIUO.immS = S() // expected-error{{cannot assign}}
immIUO.mutS = S() // expected-error{{cannot assign}}
immIUO.immS = S() // expected-error{{cannot assign}}

func foo(x: Int) {}

var nonOptional: S = S()
_ = nonOptional! // expected-error{{operand of postfix '!' should have optional type; type is 'S'}}
_ = nonOptional!.x // expected-error{{operand of postfix '!' should have optional type; type is 'S'}}

class C {}
class D: C {}

let c = C()
let d = (c as! D)! // expected-error{{forced downcast already produces a non-optional value}}
