// RUN: %target-typecheck-verify-swift

func foo(chr: Character) -> String {
  return String(repeating: String(chr)) // expected-error {{argument labels '(repeating:)' do not match any available overloads}}
  // expected-note@-1 {{overloads for 'String' exist with these partially matching parameter lists}}
}
