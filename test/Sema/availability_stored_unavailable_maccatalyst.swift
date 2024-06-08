// RUN: %target-typecheck-verify-swift -target %target-cpu-apple-ios13.1-macabi -parse-as-library -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -parse-stdlib

// REQUIRES: OS=macosx

struct BadStruct {
  @available(macCatalyst 13.1, *)
  @available(iOS, unavailable) // This attribute is inactive so we don't expect a diagnostic
  var availableOnCatalyst: Builtin.Int1

  @available(macCatalyst, unavailable) // expected-error {{stored properties cannot be marked unavailable with '@available'}}
  @available(iOS 13, *)
  var unavailableOnCatalyst: Builtin.Int1
}
