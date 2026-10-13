// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-disabled-eh.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check-objc.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-objc-interop -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check-objc.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fno-objc-exceptions -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check-methods.swift -I %t/methods -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -verify -verify-ignore-unrelated

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx

//--- check-objc.swift
import AnnotatedThrows

checked() // expected-error {{'checked()' is unavailable: SWIFT_THROWS requires Objective-C interoperability and exception handling on Darwin}}

//--- methods/module.modulemap
module AnnotatedMethods {
  header "methods.h"
  requires objc, cplusplus
}

//--- methods/methods.h
__attribute__((objc_root_class))
@interface Methods
- (int)checked __attribute__((swift_attr("import_throws")));
+ (int)classChecked __attribute__((swift_attr("import_throws")));
@end

//--- check-methods.swift
import AnnotatedMethods

func check(_ methods: Methods) {
  _ = methods.checked() // expected-error {{'checked()' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
  _ = Methods.classChecked() // expected-error {{'classChecked()' is unavailable: SWIFT_THROWS is not supported on this kind of declaration}}
}
