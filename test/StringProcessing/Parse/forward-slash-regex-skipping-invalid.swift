// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-all-function-bodies
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-non-inlinable-function-bodies-without-types
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking -experimental-skip-non-inlinable-function-bodies

// REQUIRES: swift_in_compiler

// We don't consider this a regex literal when skipping as it has an initial
// space.
func a() { _ = / x*/ } // expected-error {{unexpected end of block comment}}

// Same because of unbalanced ')'
func b() { _ = /x)*/ } // expected-error {{unexpected end of block comment}}

// These also fail the heuristic, but have unbalanced `{` `}`, so we don't skip.
func c() { _ = / x}*/ } // expected-error {{regex literal may not start with space; add backslash to escape}}
func d() { _ = / x{*/ } // expected-error {{regex literal may not start with space; add backslash to escape}}

// Unterminated, and unbalanced `{}`.
func e() {
  _ = /         }
  // expected-error@-1 {{unterminated regex literal}}
  // expected-error@-2 {{regex literal may not start with space; add backslash to escape}}
}
func f() {
  _ = /         {
  // expected-error@-1 {{unterminated regex literal}}
  // expected-error@-2 {{regex literal may not start with space; add backslash to escape}}
}
func g() {
  _ = /x         }
} // expected-error {{extraneous '}' at top level}}
func h() {
  _ = /x         {
  } // The above cannot a regex literal so we skip; this `}` is to balance things out.
}
func i() {
  _ = /x "[abc]     {
  // expected-error@-1 {{unterminated string literal}}
}
func j() {
  _ = /^ [abc]     {
  // expected-error@-1 {{unterminated regex literal}}
}
func k() {
  _ = /^ "[abc]     {
  // expected-error@-1 {{unterminated string literal}}
}
func l() {
  _ = /^    } abc     {
  // expected-error@-1 {{unterminated regex literal}}
}
func m() {
  _ = / "
  // expected-error@-1 {{unterminated string literal}}
  }
} // expected-error {{extraneous '}' at top level}}

// Unbalanced `}`, make sure we don't consider the string literal `{`.
func n() { / "{"}/ } // expected-error {{regex literal may not start with space; add backslash to escape}}

func err1() { _ = / 0xG}/ }
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
func err2() { _ = / 0oG}/ }
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
func err3() { _ = / {"/ }
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
func err4() { _ = / {'/ }
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
func err5() { _ = / {<#placeholder#>/ }
// expected-error@-1 {{regex literal may not start with space; add backslash to escape}}
