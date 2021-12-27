// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

let r0 = '/./'
let _: Regex<()> = r0

func takesRegex<Match>(_: Regex<Match>) {}
takesRegex('//') // okay

let r1 = '/.(.)/'
// Note: We test its type with a separate statement so that we know the type
// checker inferred the regex's type independently without contextual types.
let _: Regex<Substring>.Type = type(of: r1)

struct S {}
// expected-error @+2 {{cannot assign value of type 'Regex<Substring>' to type 'Regex<S>'}}
// expected-note @+1 {{arguments to generic parameter 'Capture' ('Substring' and 'S') are expected to be equal}}
let r2: Regex<S> = '/.(.)/'

let r3 = '/(.)(.)/'
let _: Regex<(Substring, Substring)>.Type = type(of: r3)

let r4 = '/(?<label>.)(.)/'
let _: Regex<(label: Substring, Substring)>.Type = type(of: r4)

let r5 = '/(.(.(.)))/'
let _: Regex<(Substring, Substring, Substring)>.Type = type(of: r5)

let r6 = '/(?'we'.(?'are'.(?'regex'.)))/'
let _: Regex<(we: Substring, are: Substring, regex: Substring)>.Type = type(of: r6)

let r7 = '/(?:(?:(.(.(.)*)?))*?)?/'
//               ^ 1
//                 ^ 2
//                   ^ 3
let _: Regex<([Substring]?, [Substring?]?, [[Substring]?]?)>.Type = type(of: r7)

let r8 = '/well(?<theres_no_single_element_tuple_what_can_we>do)/'
let _: Regex<Substring>.Type = type(of: r8)

let r9 = '/(a)|(b)|(c)|d/'
let _: Regex<(Substring?, Substring?, Substring?)>.Type = type(of: r9)

let r10 = '/(a)|b/'
let _: Regex<Substring?>.Type = type(of: r10)
