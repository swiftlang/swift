// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: libswift

let r0 = '.' // okay

let r1: Regex<(Substring, DynamicCaptures)> = '.(.)' // okay

// expected-error @+2 {{cannot assign value of type 'Regex<(Substring, DynamicCaptures)>' to type 'Regex<Substring>'}}
// expected-note @+1 {{arguments to generic parameter 'Match' ('(Substring, DynamicCaptures)' and 'Substring') are expected to be equal}}
let r2: Regex<Substring> = '.(.)'

func takesRegex<Match>(_: Regex<Match>) {}
takesRegex('') // okay
