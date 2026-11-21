// RUN: %target-swift-frontend -typecheck -verify %s

// If @main suppressed parsing of top level code parsing, we should diagnose.

@main
struct Entry {
  static func main() {}
}

1 + 1 // expected-error {{expressions are not allowed at the top level}}
// expected-warning@-1 {{result of operator '+' is unused}}

if Bool.random() {} // expected-error {{statements are not allowed at the top level}}

func isAsync() async -> Int { 42 }
let rider = await isAsync() // expected-error {{'async' call cannot occur in a global variable initializer}}
