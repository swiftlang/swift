// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=swift-5.9
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=swift-6
// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import TypeClassification

func test() {
  let x = StructWithPointerNonCopyableTriviallyMovable()
  let v = copy x // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = v
}

func testField() {
  let x = StructWithPointerNonCopyableTriviallyMovableField()
  let v = copy x // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = v
}

func testAnnotated() {
  let x = StructCopyableMovableAnnotatedNonCopyable()
  let v = copy x // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = v
}

test()
testField()
testAnnotated()
