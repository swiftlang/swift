// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// These errors are fatal, so test each one separately

// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/a.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/b.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/c.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/d.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/e.swift -verify-additional-prefix aliased- -module-alias FooBar=std_foo_bar
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/e.swift -verify-additional-prefix unaliased-
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/f.swift -verify-additional-prefix aliased- -module-alias X=std_x_h
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default %t/f.swift -verify-additional-prefix unaliased-

//--- a.swift
import std // expected-error{{no such module 'std'}}
           // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-11=CxxStdlib}} {{none}}

//--- b.swift
import std_ // expected-error{{no such module 'std_'}}
            // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-12=CxxStdlib}} {{none}}

//--- c.swift
import std_error_h // expected-error{{no such module 'std_error_h'}}
                   // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-19=CxxStdlib}} {{none}}

//--- d.swift
import std_core.math.abs // expected-error{{no such module 'std_core.math.abs'}}
                         // expected-note@-1{{did you mean 'CxxStdlib'?}}{{8-16=CxxStdlib}} {{none}}

//--- e.swift
import FooBar // expected-aliased-error{{no such module 'std_foo_bar'}}
              // expected-aliased-note@-1{{did you mean 'CxxStdlib'?}}{{8-14=CxxStdlib}} {{none}}
              // expected-aliased-note@-2{{module name 'FooBar' is aliased to 'std_foo_bar'}} {{none}}
              // expected-unaliased-error@-3{{no such module 'FooBar'}}

//--- f.swift
import X
// expected-aliased-error@-1{{no such module 'std_x_h'}}
// expected-aliased-note@-2{{did you mean 'CxxStdlib'?}}{{8-9=CxxStdlib}} {{none}}
// expected-aliased-note@-3{{module name 'X' is aliased to 'std_x_h'}} {{none}}
// expected-unaliased-error@-4{{no such module 'X'}}
