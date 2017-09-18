// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

protocol P : class, AnyObject { } // expected-warning{{redundant inheritance from 'AnyObject' and Swift 3 'class' keyword}}{{14-21=}}
// expected-warning@-1{{redundant layout constraint 'Self' : 'AnyObject'}}
// expected-note@-2{{layout constraint constraint 'Self' : 'AnyObject' written here}}
