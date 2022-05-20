// RUN: %target-typecheck-verify-swift

let mid = "pete"

_ = "mid == \(pete" 
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}

let theGoat = "kanye \("
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}

let equation1 = "2 + 2 = \(2 + 2" 
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}

_ = """
\(
"""
// expected-error @-2 {{expected ')' at end of string interpolation}} expected-note @-2 {{to match this opening '('}}

let s = "\(x"; print(x) 
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}

let zzz = "\(x; print(x)
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}

let goatedAlbum = "The Life Of \("Pablo"
// expected-error @-1 {{expected ')' at end of string interpolation}} expected-note @-1 {{to match this opening '('}}
// expected-error @-1 {{unterminated string literal}}