// RUN: %target-typecheck-verify-swift

func foo(x: Int) {}
foo(: 1) // expected-error{{expected argument label before colon}} expected-error{{missing argument label 'x:' in call}}
