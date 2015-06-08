// RUN: %target-build-swift -parse %s -Xfrontend -verify
// RUN: %target-build-swift -parse -parse-as-library %s -Xfrontend -verify
// REQUIRES: executable_test

@objc class Foo {} // expected-error {{@objc attribute used without importing module 'Foundation'}}

class Oof {
  dynamic func impliesObjC() { } // expected-error {{dynamic attribute used without importing module 'Foundation'}}
}
