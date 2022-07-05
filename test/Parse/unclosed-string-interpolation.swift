// RUN: %target-typecheck-verify-swift

let mid = "pete"

_ = "mid == \(pete" 
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

let theGoat = "kanye \("
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

let equation1 = "2 + 2 = \(2 + 2" 
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

let s = "\(x"; print(x) 
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

let zzz = "\(x; print(x)
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

let goatedAlbum = "The Life Of \("Pablo"
// expected-error @-1 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @-2 {{unterminated string literal}}

// expected-error @+3 {{cannot find ')' to match opening '(' in string interpolation}}
// expected-error @+1 {{unterminated string literal}}
_ = """
\(
"""