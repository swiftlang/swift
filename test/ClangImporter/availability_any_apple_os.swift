// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated -I %t %t/main.swift -target %target-swift-6.2-abi-triple -verify-additional-prefix %target-os-

// REQUIRES: OS=macosx || OS=ios || OS=linux-gnu || OS=windows-msvc

//--- module.modulemap

module AnyAppleOS {
  header "AnyAppleOS.h"
  export *
}

//--- AnyAppleOS.h

void introduced_in_26_2() __attribute__((availability(anyappleos, introduced = 26.2)));
void deprecated_in_26_0() __attribute__((availability(anyappleos, deprecated = 26.0)));
void obsoleted_in_26_0() __attribute__((availability(anyappleos, obsoleted = 26.0)));
void unavailable() __attribute__((availability(anyappleos, unavailable)));

//--- main.swift

import AnyAppleOS

func test() {
  // expected-macosx-note@-1 {{add '@available' attribute to enclosing global function}}{{1-1=@available(macOS 26.2, *)\n}}
  // expected-ios-note@-2 {{add '@available' attribute to enclosing global function}}{{1-1=@available(iOS 26.2, *)\n}}
  introduced_in_26_2()
  // expected-macosx-error@-1 {{'introduced_in_26_2()' is only available in macOS 26.2 or newer}}
  // expected-macosx-note@-2 {{add 'if #available' version check}}{{3-23=if #available(macOS 26.2, *) {\n      introduced_in_26_2()\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  // expected-ios-error@-3 {{'introduced_in_26_2()' is only available in iOS 26.2 or newer}}
  // expected-ios-note@-4 {{add 'if #available' version check}}{{3-23=if #available(iOS 26.2, *) {\n      introduced_in_26_2()\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  deprecated_in_26_0()
  // expected-macosx-warning@-1 {{'deprecated_in_26_0()' was deprecated in any Apple OS 26.0}}
  // expected-ios-warning@-2 {{'deprecated_in_26_0()' was deprecated in any Apple OS 26.0}}

  obsoleted_in_26_0()
  // expected-macosx-error@-1 {{'obsoleted_in_26_0()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'obsoleted_in_26_0()' is unavailable in iOS}}

  unavailable()
  // expected-macosx-error@-1 {{'unavailable()' is unavailable in macOS}}
  // expected-ios-error@-2 {{'unavailable()' is unavailable in iOS}}
}
