// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

/// Round-trip @objcDirect through a binary module; without the attribute, the
/// conformance still works.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Library with the attribute, emitted as a binary .swiftmodule.
// RUN: %target-swift-frontend -emit-module -o %t/DirectLib.swiftmodule \
// RUN:   -module-name DirectLib -enable-experimental-feature ObjCDirect \
// RUN:   %t/DirectLib.swift

// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -I %t -verify-additional-prefix direct- \
// RUN:   -enable-experimental-feature ObjCDirect %t/Client.swift

/// Control: the same library with plain @objc.
// RUN: %empty-directory(%t/plain)
// RUN: sed 's/@objcDirect/@objc/' %t/DirectLib.swift > %t/plain/DirectLib.swift
// RUN: %target-swift-frontend -emit-module -o %t/plain/DirectLib.swiftmodule \
// RUN:   -module-name DirectLib %t/plain/DirectLib.swift
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -I %t/plain -enable-experimental-feature ObjCDirect %t/Client.swift

//--- DirectLib.swift

import Foundation

@objc public protocol Greeter {
  func greet()
}

@objc public class DirectGreeter: NSObject {
  @objcDirect public final func greet() {}
}

//--- Client.swift

import DirectLib

extension DirectGreeter: @retroactive Greeter {}
// expected-direct-error@-1 {{cannot satisfy a requirement of '@objc' protocol 'Greeter' because it is a direct method}}
// expected-direct-note@DirectLib.DirectGreeter.greet:2 {{'greet()' declared here}}
// expected-direct-note@DirectLib.Greeter.greet:2 {{requirement 'greet()' declared here}}
