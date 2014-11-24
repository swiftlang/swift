// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -verify

@objc class ObjCBox<T> {
  var x: T // expected-error{{unimplemented}}

  init(x: T) {
    self.x = x
  }
}
