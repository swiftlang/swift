// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift -enable-library-evolution
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t/resilient_objc_class.swiftmodule %S/../Inputs/resilient_objc_class.swift -I %t -enable-library-evolution -target %target-next-stable-abi-triple
// RUN: %target-swift-frontend -typecheck -verify %s -I %t -target %target-next-stable-abi-triple

// REQUIRES: objc_interop
// REQUIRES: swift_stable_abi

import Foundation
import resilient_objc_class

// When built with a sufficiently new deployment target, all of these cases are
// allowed.

@objc public class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

public class AnotherResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

extension ResilientNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
}

extension AnotherResilientNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
}

// Note: @_fixed_layout on a class only applies to the storage layout and
// not metadata, which remains resilient.

@_fixed_layout
@objc public class FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

@_fixed_layout
public class AnotherFixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

extension FixedLayoutNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
}

extension AnotherFixedLayoutNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
}
