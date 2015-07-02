// RUN: %target-parse-verify-swift

import StdlibUnittest

struct S1 {}
struct S2 {}

func test_expectType() {
  var s1 = S1()
  expectType(S1.self, &s1)
  expectType(S2.self, &s1) // expected-error {{cannot invoke 'expectType' with an argument list of type '(S2.Type, inout S1)'}} expected-note {{expected an argument list of type}}
}

func test_expectEqualType() {
  expectEqualType(S1.self, S1.self)
  expectEqualType(S1.self, S2.self) // expected-error {{cannot invoke 'expectEqualType' with an argument list of type '(S1.Type, S2.Type)'}} expected-note {{expected an argument list of type}}
}

