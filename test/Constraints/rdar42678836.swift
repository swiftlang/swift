// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

func foo(chr: Character) -> String {
  return String(repeating: String(chr)) // expected-error {{missing argument for parameter 'count' in call}} {{39-39=, count: <#Int#>}}
}
