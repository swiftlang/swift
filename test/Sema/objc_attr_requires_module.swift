// RUN: %swift_driver -parse %s -Xfrontend -verify
@objc class Foo {} // expected-error {{@objc attribute used without importing module 'Foundation'}}
