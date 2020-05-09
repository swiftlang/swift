// RUN: %target-typecheck-verify-swift -swift-version 4
// RUN: not %target-swift-frontend -typecheck -swift-version 5

protocol P : class, AnyObject { } // expected-warning{{redundant inheritance from 'AnyObject' and Swift 3 'class' keyword}}{{14-21=}}
// expected-warning@-1{{redundant constraint 'Self' : 'AnyObject'}}
// expected-note@-2{{constraint 'Self' : 'AnyObject' written here}}
