// RUN: %empty-directory(%t)
// RUN: split-file %S/annotated-throws-disabled-eh.swift %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check-objc.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -disable-objc-interop -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check-objc.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fno-objc-exceptions -verify -verify-ignore-unrelated

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx

//--- check-objc.swift
import AnnotatedThrows

checked() // expected-error {{'checked()' is unavailable: SWIFT_THROWS requires Objective-C interoperability and exception handling on Darwin}}
