// RUN: %target-parse-verify-swift

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

struct S1 {}
struct S2 {}

func test_expectType() {
  var s1 = S1()
  expectType(S1.self, &s1)
  expectType(S2.self, &s1) // expected-error {{cannot convert value of type 'S1' to expected argument type 'S2'}}
}

func test_expectEqualType() {
  expectEqualType(S1.self, S1.self)
  expectEqualType(S1.self, S2.self) // expected-error {{cannot convert value of type 'S2.Type' to expected argument type 'S1.Type'}}
}

