// RUN: %swift %s -verify
//
// Test recovery for 'self' and 'Self' keywords used in inappropriate places.

func self() {} // expected-error {{expected identifier in func declaration}}
func Self() {} // expected-error {{expected identifier in func declaration}}

struct Self {} // FIXME

func freeFunc1() -> self {} // expected-error {{expected type}} expected-error {{consecutive statements on a line must be separated by ';'}} expected-error {{use of unresolved identifier 'self'}}
func freeFunc2() -> Self {} // FIXME

func freeFunc3(self: Int) {} // expected-error {{expected pattern is a keyword}}
func freeFunc4(Self: Int) {} // expected-error {{expected pattern is a keyword}}

func freeFunc5(a: self) {} // expected-error {{expected pattern is a keyword}} expected-error {{expected type}} expected-error {{expected ',' separator}}
func freeFunc6(a: Self) {} // FIXME

struct Structs {
  struct self {} // FIXME
  struct Self {} // FIXME
}

struct Unions {
  union self {} // FIXME
  union Self {} // FIXME
}

struct Classes { // expected-note {{to match this opening '{'}}
  class self {} // FIXME
  class Self {} // FIXME
  extension self {} // expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error {{expected declaration}} expected-error {{expected type}}
  extension Self {} // expected-error {{expected '}' in struct}} FIXME: what about Self?
} //expected-error {{extraneous '}' at top level}}

struct Protocols {
  // FIXME: this errors out for the wrong reason
  protocol self {} // expected-error {{declaration is only valid at file scope}}
  protocol Self {} // expected-error {{declaration is only valid at file scope}}
}

struct Typealiases {
  typealias self = Int // FIXME
  typealias Self = Int // FIXME
}

var self = 123 // expected-error {{expected pattern is a keyword}}
var Self = 123 // expected-error {{expected pattern is a keyword}}

struct FooStruct {
  var self : Int // expected-error {{expected pattern is a keyword}}
  var Self : Int // expected-error {{expected pattern is a keyword}}
}

//union FooUnion {
  // FIXME: this diagnostic is misleading.
//  case self(Int) // e/xpected-error {{'case' label can only appear inside a 'switch' statement}}
//  case Self(Int)
//}
// FIXME: this diagnostic is produced on a line after the last one, thus the
// whole union testcase is disabled.
// e/xpected-error {{expected declaration}}

