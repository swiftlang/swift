// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -target %target-swift-5.7-abi-triple -experimental-skip-all-function-bodies
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -target %target-swift-5.7-abi-triple -experimental-skip-non-inlinable-function-bodies-without-types
// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -target %target-swift-5.7-abi-triple -experimental-skip-non-inlinable-function-bodies

// REQUIRES: swift_swift_parser

// We don't consider this a regex literal when skipping as it has an initial
// space.
func a() { _ = / x*/ } // expected-cparser-error {{unexpected end of block comment}} expected-astgen-error {{bare slash regex literal may not start with space}}

// Same because of unbalanced ')'
func b() { _ = /x)*/ } // expected-cparser-error {{unexpected end of block comment}}

// These also fail the heuristic, but have unbalanced `{` `}`, so we don't skip.
func c() { _ = / x}*/ } // expected-cparser-error {{regex literal may not start with space; add backslash to escape}} expected-astgen-error {{bare slash regex literal may not start with space}}
func d() { _ = / x{*/ } // expected-cparser-error {{regex literal may not start with space; add backslash to escape}} expected-astgen-error {{bare slash regex literal may not start with space}}

// Unterminated, and unbalanced `{}`.
func e() {
  _ = /         } // expected-astgen-note {{to match this opening '/'}}
  // expected-cparser-error@-1 {{unterminated regex literal}} expected-astgen-error@-1 {{expected '/' to end regex literal}}
  // expected-cparser-error@-2 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-2 {{bare slash regex literal may not start with space}}
}
func f() {
  _ = /         { // expected-astgen-note {{to match this opening '/'}}
  // expected-cparser-error@-1 {{unterminated regex literal}} expected-astgen-error@-1 {{expected '/' to end regex literal}}
  // expected-cparser-error@-2 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-2 {{bare slash regex literal may not start with space}}
}
func g() {
  _ = /x         }
} // expected-cparser-error {{extraneous '}' at top level}} expected-astgen-error {{unexpected brace in source file}}
func h() {
  _ = /x         {
  } // The above cannot a regex literal so we skip; this `}` is to balance things out.
}
func i() {
  _ = /x "[abc]     {
  // expected-cparser-error@-1 {{unterminated string literal}}
}
func j() {
  _ = /^ [abc]     { // expected-astgen-note {{to match this opening '/'}}
  // expected-cparser-error@-1 {{unterminated regex literal}} expected-astgen-error@-1 {{expected '/' to end regex literal}}
}
func k() {
  _ = /^ "[abc]     { // expected-astgen-note {{to match this opening '/'}}
  // expected-cparser-error@-1 {{unterminated string literal}} expected-astgen-error@-1 {{expected '/' to end regex literal}}
}
func l() {
  _ = /^    } abc     { // expected-astgen-note {{to match this opening '/'}}
  // expected-cparser-error@-1 {{unterminated regex literal}} expected-astgen-error@-1 {{expected '/' to end regex literal}}
}
func m() {
  _ = / " // expected-astgen-note {{to match this opening '/'}} expected-astgen-error {{expected '/' to end regex literal}}
  // expected-cparser-error@-1 {{unterminated string literal}} expected-astgen-error@-1 {{bare slash regex literal may not start with space}}
  }
} // expected-cparser-error {{extraneous '}' at top level}} expected-astgen-error {{unexpected brace in source file}}

// Unbalanced `}`, make sure we don't consider the string literal `{`.
func n() { / "{"}/ } // expected-cparser-error {{regex literal may not start with space; add backslash to escape}} expected-astgen-error {{bare slash regex literal may not start with space}}

func o() {
  _ = {
    0
    /x}}} /
    2
  } // expected-cparser-error {{extraneous '}' at top level}} expected-astgen-error {{unexpected braces in source file}}
  // expected-cparser-error@-3 {{extraneous '}' at top level}}
  // expected-cparser-error@-4 {{consecutive statements on a line must be separated by ';'}}
  // expected-cparser-error@-5 {{unterminated regex literal}} expected-astgen-error@-5 {{unexpected code '} /' in source file}}
  // expected-cparser-warning@-6 {{regular expression literal is unused}}
  // expected-warning@-6 {{integer literal is unused}}
} // expected-cparser-error {{extraneous '}' at top level}}

func p() {
  _ = 2
  /x} /
    .bitWidth
  // expected-cparser-error@-2 {{consecutive statements on a line must be separated by ';'}}
  // expected-cparser-error@-3 {{unterminated regex literal}} expected-astgen-error@-3 {{unexpected code '/' in source file}}
  // expected-cparser-error@-3 {{value of type 'Regex<Substring>' has no member 'bitWidth'}} expected-astgen-error@-3 {{reference to member 'bitWidth' cannot be resolved without a contextual type}}
} // expected-cparser-error {{extraneous '}' at top level}} expected-astgen-error {{unexpected brace in source file}}

func err1() { _ = / 0xG}/ }
// expected-cparser-error@-1 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-1 {{}}
func err2() { _ = / 0oG}/ }
// expected-cparser-error@-1 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-1 {{bare slash regex literal may not start with space}}
func err3() { _ = / {"/ }
// expected-cparser-error@-1 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-1 {{bare slash regex literal may not start with space}}
func err4() { _ = / {'/ }
// expected-cparser-error@-1 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-1 {{bare slash regex literal may not start with space}}
func err5() { _ = / {<#placeholder#>/ }
// expected-cparser-error@-1 {{regex literal may not start with space; add backslash to escape}} expected-astgen-error@-1 {{bare slash regex literal may not start with space}}
