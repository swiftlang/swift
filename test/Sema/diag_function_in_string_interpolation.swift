// RUN: %target-typecheck-verify-swift

func f() {
}

print("\(f)")
// expected-warning@-1 {{string interpolation produces a debug description for a function value; did you mean to make this explicit?}}
// expected-note@-2 {{use 'String(describing:)' to silence this warning}} {{10-10=String(describing: }} {{11-11=)}}

print("\(String(describing: f))") // No warning
