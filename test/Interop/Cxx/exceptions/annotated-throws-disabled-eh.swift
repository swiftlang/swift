// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fno-exceptions -verify -verify-ignore-unrelated
// RUN: %target-swift-frontend -typecheck %t/check.swift -I %t -cxx-interoperability-mode=default -enable-experimental-feature CxxExceptionBridging -Xcc -fignore-exceptions -verify -verify-ignore-unrelated

// REQUIRES: swift_feature_CxxExceptionBridging
// REQUIRES: OS=macosx || OS=linux-gnu

//--- module.modulemap
module AnnotatedThrows {
  header "functions.h"
  requires cplusplus
}

//--- functions.h
void checked() __attribute__((swift_attr("import_throws")));

//--- check.swift
import AnnotatedThrows

checked() // expected-error {{'checked()' is unavailable: SWIFT_THROWS requires C++ exceptions to be enabled}}
