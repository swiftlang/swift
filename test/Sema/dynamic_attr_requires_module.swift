// RUN: %target-build-swift -parse %s -Xfrontend -verify
// RUN: %target-build-swift -parse -parse-as-library %s -Xfrontend -verify
// REQUIRES: executable_test

class Oof {
  dynamic func impliesObjC() { } // expected-error {{'dynamic' attribute used without importing module 'Foundation'}}
}
