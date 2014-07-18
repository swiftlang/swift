// RUN: %swift -enable-optional-lvalues -parse -verify %s

struct S {
  var x: Int = 0
  let y: Int = 0

  init() {}
}

struct T {
  var mutS: S? = nil
  let immS: S? = nil

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

/* TODO: Implicit forcing of IUO
mutIUO.mutS = S()
mutIUO.immS = S() // e/xpected-error{{cannot assign}}
immIUO.mutS = S() // e/xpected-error{{cannot assign}}
immIUO.immS = S() // e/xpected-error{{cannot assign}}
*/
