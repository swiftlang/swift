// RUN: %target-typecheck-verify-swift -language-mode 4 -verify-additional-prefix swift4-
// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift%language_mode-

protocol P : class, AnyObject { } // expected-swift4-warning{{redundant inheritance from 'AnyObject' and Swift 3 'class' keyword}}{{14-21=}}
// expected-swift5-warning@-1{{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}}{{14-19=AnyObject}}
// expected-swift5-error@-2{{duplicate inheritance from 'AnyObject'}}{{14-21=}}

