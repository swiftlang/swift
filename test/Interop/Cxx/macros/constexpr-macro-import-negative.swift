// RUN: %target-swift-frontend -typecheck -I %S/Inputs %s -enable-experimental-cxx-interop -verify -verify-ignore-unrelated

// REQUIRES: objc_interop

import ConstexprMacros

func testNegativeCases() {
  _ = EMPTY_MACRO             // expected-error{{cannot find 'EMPTY_MACRO' in scope}}
  _ = FUNC_LIKE_MACRO_ONLY(1) // expected-error{{cannot find 'FUNC_LIKE_MACRO_ONLY' in scope}}
  _ = MACRO_REF_CONSTEXPR     // expected-error{{cannot find 'MACRO_REF_CONSTEXPR' in scope}}
  _ = MACRO_REF_ENUM          // expected-error{{cannot find 'MACRO_REF_ENUM' in scope}}
}
