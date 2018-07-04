// RUN: %target-swift-frontend -typecheck -verify -enable-objc-interop %s

class Foo {}
_ = Foo()

@objc class NSFoo {} // expected-error {{@objc attribute used without importing module 'Foundation'}} expected-error {{only classes that inherit from NSObject can be declared @objc}} {{1-7=}}
