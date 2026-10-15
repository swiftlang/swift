// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h

// REQUIRES: swift_feature_Embedded

func create() -> __swift_thread_t? {
  var thread: __swift_thread_t? = nil
  _ = _swift_thread_create(&thread, { _ in nil }, nil)
  return thread
}

func join(_ thread: __swift_thread_t?) {
  _swift_thread_join(nil) // expected-error {{'nil' is not compatible with expected argument type '__swift_thread_t'}}
  _swift_thread_join(thread) // expected-error {{value of optional type '__swift_thread_t?'}} expected-note {{coalesce}} expected-note {{force-unwrap}}
  _swift_thread_join(thread!)
}
