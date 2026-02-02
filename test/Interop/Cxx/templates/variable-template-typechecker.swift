// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -I %S/Inputs

import VariableTemplate

let _: CInt = template_gcd // expected-error {{cannot find 'template_gcd' in scope}}
