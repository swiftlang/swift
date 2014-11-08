// RUN: %swift -parse -disable-objc-attr-requires-foundation-module -primary-file %s %S/Inputs/objc_override_multi_2.swift %S/Inputs/objc_override_multi_3.swift -verify

// RUN: %swift -emit-module -emit-module-path=%t -module-name=foo -disable-objc-attr-requires-foundation-module %s %S/Inputs/objc_override_multi_2.swift %S/Inputs/objc_override_multi_3.swift -verify

class SubSub1 : Sub1 {
  @objc func a() { } // expected-error{{method 'a()' overrides Objective-C method 'a' from superclass 'Super1'}}
}

