// RUN: %target-typecheck-verify-swift

func foo(x: Int) {}
foo(: 1) // expected-cparser-error{{expected argument label before colon}} expected-astgen-error{{incorrect argument label in call (have ':', expected 'x:')}} expected-cparser-error{{missing argument label 'x:' in call}} expected-astgen-error{{expected label in function call}}
