// RUN: %target-typecheck-verify-swift

enum E1 {
  case v1
  case v2
  case v3
  case v4
  case v5
  case v6
  indirect case v7(E1)
}

enum E2 {
  case foo((E1, E1, E1)) // total size of this case is 7 ^ 3 + 1
  case bar(E1)
  case baz
}

func foo(_ e: E2) {
  switch e {
    // expected-error@-1 {{switch must be exhaustive}}
    // expected-note@-2 {{add missing case: '.bar(_)'}}

    case .foo(let tuple): break // expected-warning {{immutable value 'tuple' was never used; consider replacing with '_' or removing it}}
    case .baz: break
  }
}

func bar(_ e: E2) {
  switch e {
    // expected-error@-1 {{switch must be exhaustive}}
    // expected-note@-2 {{add missing case: '.bar(_)'}}
    // expected-note@-3 {{add missing case: '.baz'}}
    // expected-note@-4 {{add missing cases}}

    case .foo((_, _, _)): break
  }
}
