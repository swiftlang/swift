// RUN: %target-typecheck-verify-swift -swift-version 5
//
// REQUIRES: objc_interop

class Object {
  @objc dynamic func method(){
  }

  dynamic func nativeMethod() {
  }
}

extension Object {
  @_dynamicReplacement(for: method()) // expected-error{{'method()' is marked @objc dynamic}}
  func replacement() {
  }

  @_dynamicReplacement(for: nativeMethod()) // expected-error{{'nativeMethod()' is not marked @objc dynamic}}
  @objc dynamic func replacement2() {
  }

}
