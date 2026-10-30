// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/89131
//
// An unused optional binding in a multi-condition statement should offer the
// same "!= nil" boolean-test fix-it that we already emit for the
// single-condition case, instead of the generic "replace with '_'" fix-it.

func sync() -> Int? { nil }
func async() async -> Int? { nil }

func test() async {
  // Single condition (baseline — already correct before this fix).
  if let x = sync() {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{20-20= != nil}}

  // Binding followed by a boolean condition.
  if let x = sync(), true {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{20-20= != nil}}

  // Boolean condition followed by the binding.
  if true, let x = sync() {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{12-20=}} {{26-26= != nil}}

  // 'await' initializer needs surrounding parentheses.
  if let x = await async(), true {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=(}} {{27-27=) != nil}}

  // 'as?' cast in a multi-condition statement rewrites to an 'is' test.
  let v: Any = 42
  if let x = v as? Int, true {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{6-14=}} {{16-19=is}}
}

// 'guard' and 'while' share the same conditional-statement code path.
func testGuardAndWhile() {
  guard let x = sync(), true else { return }
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{9-17=}} {{23-23= != nil}}

  while let x = sync(), true {}
  // expected-warning@-1 {{value 'x' was defined but never used; consider replacing with boolean test}} {{9-17=}} {{23-23= != nil}}
}
