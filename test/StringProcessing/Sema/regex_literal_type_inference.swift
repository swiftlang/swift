// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

let r0 = '/./' // okay

let r1: Regex<DynamicCaptures> = '/.(.)/' // okay

// expected-error @+2 {{cannot assign value of type 'Regex<DynamicCaptures>' to type 'Regex<Substring>'}}
// expected-note @+1 {{arguments to generic parameter 'Capture' ('DynamicCaptures' and 'Substring') are expected to be equal}}
let r2: Regex<Substring> = '/.(.)/'

func takesRegex<Match>(_: Regex<Match>) {}
takesRegex('//') // okay
