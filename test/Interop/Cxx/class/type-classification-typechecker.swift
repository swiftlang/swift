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

func testHasCopyTypeOperations() {
  let x = HasCopyConstructorWithDefaultArgs(5)
  let v = copy x // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = v
}

func testFuncCallNoncopyable() {
  func aux(input: HasCopyConstructorWithDefaultArgs) {}
  let x = HasCopyConstructorWithDefaultArgs(5)
  aux(input: x) 
  // expected-error@-3 {{parameter of noncopyable type 'HasCopyConstructorWithDefaultArgs' must specify ownership}}
  // expected-note@-4 {{add 'borrowing' for an immutable reference}}
  // expected-note@-5 {{add 'consuming' to take the value from the caller}}
  // expected-note@-6 {{add 'inout' for a mutable reference}}
}

func testHasMoveTypeOperations() {
  let x = HasMoveConstructorWithDefaultArgs(5) // expected-error {{cannot find 'HasMoveConstructorWithDefaultArgs' in scope}}
}

func testHasCopyOrMoveTypeOperations() {
  let x = HasCopyAndMoveConstructorWithDefaultArgs(5) 
  // expected-error@-1 {{cannot find 'HasCopyAndMoveConstructorWithDefaultArgs' in scope}}
}

test()
testField()
testAnnotated()
testHasCopyTypeOperations()
testFuncCallNoncopyable()
testHasMoveTypeOperations()
testHasCopyOrMoveTypeOperations()
