// RUN: %target-swift-frontend -typecheck -enable-objc-interop -disable-objc-attr-requires-foundation-module -primary-file %s %S/Inputs/objc_override_multi_2.swift %S/Inputs/objc_override_multi_3.swift -verify

// RUN: %target-swift-frontend -emit-module -enable-objc-interop -emit-module-path=%t -module-name=foo -disable-objc-attr-requires-foundation-module %s %S/Inputs/objc_override_multi_2.swift %S/Inputs/objc_override_multi_3.swift -verify

class SubSub1 : Sub1 {
  @objc func a() { } // expected-error{{method 'a()' with Objective-C selector 'a' conflicts with method 'f()' from superclass 'Super1' with the same Objective-C selector}}

  func test() {
    // Needed to force enough checking of Super1.f() to record its
    // Objective-C selector.
    f()
  }
}

