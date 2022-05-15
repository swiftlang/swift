// RUN: %target-typecheck-verify-swift

let mid = "pete"

_ = "mid == \(pete" // expected-error {{expected ')' at end of string interpolation}}
let theGoat = "kanye \(" // expected-error {{expected ')' at end of string interpolation}}
let equation1 = "2 + 2 = \(2 + 2" // expected-error {{expected ')' at end of string interpolation}}

_ = """
\(
"""
// expected-error @-2 {{expected ')' at end of string interpolation}}