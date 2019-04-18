// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift -enable-library-evolution
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t/resilient_objc_class.swiftmodule %S/../Inputs/resilient_objc_class.swift -I %t -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %s -I %t

// REQUIRES: objc_interop

import Foundation
import resilient_objc_class

@objc public class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}
// expected-error@-1 {{classes built with library evolution support cannot have explicit '@objc' subclasses because they are not directly visible from Objective-C}}

public class AnotherResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

extension ResilientNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
  // expected-error@-1 {{extensions of classes built with library evolution support cannot contain '@objc' members}}
}

extension AnotherResilientNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
  // expected-error@-1 {{extensions of classes built with library evolution support cannot contain '@objc' members}}
}

// Note: @_fixed_layout on a class only applies to the storage layout and
// not metadata, which remains resilient.

@_fixed_layout
@objc public class FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}
// expected-error@-1 {{classes built with library evolution support cannot have explicit '@objc' subclasses because they are not directly visible from Objective-C}}

@_fixed_layout
public class AnotherFixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

extension FixedLayoutNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
  // expected-error@-1 {{extensions of classes built with library evolution support cannot contain '@objc' members}}
}

extension AnotherFixedLayoutNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
  // expected-error@-1 {{extensions of classes built with library evolution support cannot contain '@objc' members}}
}
