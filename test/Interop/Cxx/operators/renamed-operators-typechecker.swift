// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import RenamedOperators

let star = HasRenamedOperatorStar(value: 123)
_ = *star // expected-error {{'*' is not a prefix unary operator}}

let plusPlus = HasRenamedOperatorStar(value: 123)
plusPlus++ // expected-error {{cannot find operator '++' in scope; did you mean '+= 1'?}}
