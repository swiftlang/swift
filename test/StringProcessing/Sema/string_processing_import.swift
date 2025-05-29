// RUN: %target-typecheck-verify-swift -disable-implicit-string-processing-module-import -disable-availability-checking
// REQUIRES: swift_swift_parser

// expected-error @+1 {{missing 'Regex' declaration, probably because the '_StringProcessing' module was not imported properly}}
let r0 = #/./#
// expected-error @+1 {{cannot find type 'Regex' in scope}}
let _: Regex<Substring> = r0
