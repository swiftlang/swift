// RUN: %target-build-swift -parse %s -Xfrontend -verify
// REQUIRES: executable_test
// REQUIRES: objc_interop

class Foo {}
_ = Foo()

@objc class NSFoo {} // expected-error {{@objc attribute used without importing module 'Foundation'}}
