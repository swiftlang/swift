// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the supporting modules.
// RUN: %target-swift-frontend -emit-module %t/ExtensionModule.swift -o %t \
// RUN:   -module-name ExtensionModule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/ProtoModule.swift -o %t \
// RUN:   -I %t -module-name ProtoModule -swift-version 5 -enable-library-evolution

// Typecheck the handwritten swiftinterface. This should succeed without errors
// even when MemberImportVisibility is enabled, because swiftinterfaces are
// generated from valid source and conformance witnesses should not trigger
// import visibility diagnostics.
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) \
// RUN:   -module-name Client -I %t -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- ExtensionModule.swift

extension Int {
  public func witnessMe() { }
}

//--- ProtoModule.swift

import ExtensionModule

public protocol Proto {
  func witnessMe()
}

//--- Client.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Client -enable-upcoming-feature MemberImportVisibility
import ProtoModule

extension Int: Proto {}
