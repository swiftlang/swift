// RUN: %target-typecheck-verify-swift -swift-version 3

protocol P { }
protocol Q { }

class Foo: AnyObject { } // expected-warning{{conformance of class 'Foo' to 'AnyObject' is redundant}}{{10-21=}}

class Bar: AnyObject, P { } // expected-warning{{conformance of class 'Bar' to 'AnyObject' is redundant}}{{12-23=}}

class Wibble: Bar, AnyObject, Q { } // expected-warning{{conformance of class 'Wibble' to 'AnyObject' is redundant}}{{18-29=}}
