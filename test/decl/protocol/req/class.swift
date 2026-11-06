// RUN: %target-typecheck-verify-swift

protocol P1 : class { }

protocol P2 : class, class { } // expected-cparser-error{{redundant 'class' requirement}}{{20-27=}} expected-astgen-error {{redundant 'class' requirement}}

protocol P3 : P2, class { } // expected-cparser-error{{'class' must come first in the requirement list}}{{15-15=class, }}{{17-24=}} expected-astgen-error {{'class' must come first in the requirement list}}

struct X : class { } // expected-error{{'class' constraint can only appear on protocol declarations}}
