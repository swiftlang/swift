// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -enable-library-evolution -module-name MyKitZip %S/../IRGen/Inputs/back_deployed_type_zippered_module.swift -o %t/MyKitZip.swiftmodule -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi
// RUN: %target-swift-frontend -typecheck -verify -parse-as-library -enable-library-evolution -I %t %s -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi

// REQUIRES: OS=macosx || OS=maccatalyst

// In a zippered build, a type that is back-deployed only for the target
// variant has the same restrictions as any other back-deployed type.

import MyKitZip

extension VariantOnly: @retroactive CustomStringConvertible { // expected-error {{conformance of back-deployed type 'VariantOnly' to resilient protocol 'CustomStringConvertible' must not be available before iOS 99}}
  // expected-note@-1 {{this module uses its own copy of 'VariantOnly' before iOS 99, and the conformance would not apply to 'VariantOnly' from 'MyKitZip'}}
  // expected-note@-2 {{add '@available' attribute to enclosing extension}}
  public var description: String { "" }
}

@available(macOS 10.15, iOS 99, *)
extension NeededForVariant: @retroactive CustomStringConvertible {
  public var description: String { "" }
}

extension NotNeeded: @retroactive CustomStringConvertible {
  public var description: String { "" }
}

@backDeployed(before: iOS 99)
@frozen public struct VariantOnlyHere {
  public var x: Int
  public static var stored = 0 // expected-error {{back-deployed struct 'VariantOnlyHere' cannot have static stored property 'stored'}}

  @available(macOS 10.15, iOS 99, *)
  public static var storedOptedOut = 0

  public final class Nested {} // expected-error {{class 'Nested' cannot be nested in back-deployed struct 'VariantOnlyHere'}}

  @available(iOS 99, *)
  public final class NestedOptedOut {}
}
