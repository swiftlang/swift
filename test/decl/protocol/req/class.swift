// RUN: %target-parse-verify-swift

protocol P1 : class { }

protocol P2 : class, class { } // expected-error{{redundant 'class' requirement}}{{20-27=}}

protocol P3 : P2, class { } // expected-error{{'class' must come first in the requirement list}}{{15-15=class, }}{{17-24=}}

struct X : class { } // expected-error{{'class' requirement only applies to protocols}} {{12-18=}}


// rdar://problem/21268222
class Foo<T>{}
class Bar<T, U where U : Foo<T>>{} // expected-error{{superclass constraint 'Foo<T>' cannot depend on a type parameter}}
