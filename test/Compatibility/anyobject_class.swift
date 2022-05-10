// RUN: %target-typecheck-verify-swift -swift-version 4 -warn-redundant-requirements
// RUN: not %target-swift-frontend -typecheck -swift-version 5

protocol P : class, AnyObject { } // expected-warning{{redundant inheritance from 'AnyObject' and Swift 3 'class' keyword}}{{14-21=}}
// expected-warning@-1{{redundant constraint 'Self' : 'AnyObject'}}
