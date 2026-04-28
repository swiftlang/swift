// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the supporting modules.
// RUN: %target-swift-frontend -emit-module %t/ProtoModule.swift -o %t \
// RUN:   -module-name ProtoModule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/RefinementModule.swift -o %t \
// RUN:   -I %t -module-name RefinementModule -swift-version 5 -enable-library-evolution

// Typecheck the handwritten swiftinterface. This should succeed without errors
// even when MemberImportVisibility is enabled, because swiftinterfaces are
// generated from valid source and conformance witnesses should not trigger
// import visibility diagnostics.
// RUN: %target-swift-typecheck-module-from-interface(%t/Client.swiftinterface) \
// RUN:   -module-name Client -I %t -enable-upcoming-feature MemberImportVisibility

// REQUIRES: swift_feature_MemberImportVisibility

//--- ProtoModule.swift

public protocol Proto {
  func witnessMe()
}

extension Proto {
  public func witnessMe() { }
}

//--- RefinementModule.swift

import ProtoModule

public protocol RefinedProto: Proto { }

//--- Client.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -swift-version 5 -enable-library-evolution -module-name Client -enable-upcoming-feature MemberImportVisibility
import RefinementModule

struct S: RefinedProto {}
