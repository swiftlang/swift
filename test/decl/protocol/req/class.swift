// RUN: %target-parse-verify-swift

protocol P1 : class { }

protocol P2 : class, class { } // expected-error{{redundant 'class' requirement}}{{20-27=}}

protocol P3 : P2, class { } // expected-error{{'class' must come first in the requirement list}}{{15-15=class, }}{{17-24=}}

@class_protocol protocol P4 { } // expected-error{{the 'class_protocol' attribute has been replaced with a 'class' requirement}}{{1-17=}}{{28-28= : class}}

@class_protocol protocol P5 : P4 { } // expected-error{{the 'class_protocol' attribute has been replaced with a 'class' requirement}}{{1-17=}}{{31-31=class, }}

struct X : class { } // expected-error{{'class' requirement only applies to protocols}}


