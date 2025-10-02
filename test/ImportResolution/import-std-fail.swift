// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -module-alias FooBar=std_foo_bar -module-alias X=std_x_h

import std // expected-error{{no such module 'std'}}
           // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-11=CxxStdlib}} {{none}}

import std_ // expected-error{{no such module 'std_'}}
            // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-12=CxxStdlib}} {{none}}

import std_error_h // expected-error{{no such module 'std_error_h'}}
                   // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-19=CxxStdlib}} {{none}}

import std_core.math.abs // expected-error{{no such module 'std_core.math.abs'}}
                         // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-16=CxxStdlib}} {{none}}

import FooBar // expected-error{{no such module 'std_foo_bar'}}
              // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-14=CxxStdlib}} {{none}}
              // expected-note@-2{{module name 'FooBar' is aliased to 'std_foo_bar'}} {{none}}

import X
// expected-error@-1{{no such module 'std_x_h'}}
// expected-note@-2{{did you mean 'CxxStdlib'?}}{{8-9=CxxStdlib}} {{none}}
// expected-note@-3{{module name 'X' is aliased to 'std_x_h'}} {{none}}
