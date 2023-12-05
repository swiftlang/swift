// Test various aspects of the C++ `nodiscard` keyword.

// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import TypeClassification

func test() {
  let x = StructCopyableMovableAnnotatedNonCopyable()
  let v = copy x // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = v
}

test()
