// RUN: %target-typecheck-verify-swift

func test(_ v: [Int]) {
  let result = v.filter { }.flatMap(\.wrong) {
    // expected-error@-1 {{type for closure argument list expects 1 argument, which cannot be implicitly ignored}}
    // expected-error@-2 {{cannot convert value of type '()' to closure result type 'Bool'}}
    // expected-error@-3 {{value of type 'Int' has no member 'wrong'}}
    // expected-error@-4 {{extra trailing closure passed in call}}
    print(result)
  }

  let otherResult = v.filter { _ in false }.flatMap(\.wrong, { $0 }, 42)
  // expected-error@-1 {{value of type 'Int' has no member 'wrong'}}
  // expected-error@-2 {{extra arguments at positions #2, #3 in call}}
}
