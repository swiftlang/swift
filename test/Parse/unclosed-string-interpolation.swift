// RUN: %target-typecheck-verify-swift

let mid = "pete"

_ = "mid == \(pete" // expected-error {{expected ')' at end of string interpolation}} expected-note {{to match this opening '('}}
let theGoat = "kanye \(" // expected-error {{expected ')' at end of string interpolation}} expected-note {{to match this opening '('}}
let equation1 = "2 + 2 = \(2 + 2" // expected-error {{expected ')' at end of string interpolation}} expected-note {{to match this opening '('}}

_ = """
\(
"""
// expected-error @-2 {{expected ')' at end of string interpolation}} expected-note @-2 {{to match this opening '('}}

let s = "\(x"; print(x) // expected-error {{expected ')' at end of string interpolation}} expected-note {{to match this opening '('}}
let zzz = "\(x; print(x) // expected-error {{expected ')' at end of string interpolation}} expected-note {{to match this opening '('}}