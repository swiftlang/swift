// REQUIRES: swift_swift_parser

// RUN: %target-typecheck-verify-swift -swift-version 5

@freestanding(expression) macro m6 = A
// expected-cparser-error@-1 {{expected '(' for macro parameters or ':' for a value-like macro}}
// expected-cparser-error@-2 {{macro must itself be defined by a macro expansion expression (starting with '#')}}
// expected-astgen-error@-3 {{expected parameter clause in function signature}}
// expected-astgen-error@-4 {{macro must itself be defined by a macro expansion expression (starting with '#')}}
