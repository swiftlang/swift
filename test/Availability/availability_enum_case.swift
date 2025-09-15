// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-module %S/Inputs/availability_enum_case_other.swift -target %target-cpu-apple-macosx50 -emit-module-interface-path %t/availability_enum_case_other.swiftinterface -swift-version 5 -enable-library-evolution
// RUN: %target-typecheck-verify-swift -I %t

// RUN: %target-build-swift -emit-module %S/Inputs/availability_enum_case_other.swift -target %target-cpu-apple-macosx50 -emit-module-interface-path %t/availability_enum_case_other.swiftinterface -swift-version 5 -enable-library-evolution -whole-module-optimization
// RUN: %target-typecheck-verify-swift -I %t

// REQUIRES: OS=macosx

import availability_enum_case_other

func ride(horse: Horse) {
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}

  _ = Horse.kevin
  // expected-error@-1 {{'kevin' is only available in macOS 50 or newer}}
  // expected-note@-2 {{add 'if #available' version check}}
}
