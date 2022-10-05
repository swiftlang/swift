// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift -enable-library-evolution
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module-path %t/resilient_objc_class.swiftmodule %S/../Inputs/resilient_objc_class.swift -I %t -enable-library-evolution
// RUN: %target-swift-frontend -typecheck -verify %s -I %t -target %target-cpu-apple-macosx10.13

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import resilient_objc_class

@objc public class ResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}
// expected-error@-1 {{explicit '@objc' on subclass of 'ResilientNSObjectOutsideParent' requires}}

public class AnotherResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

extension ResilientNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
  // expected-error@-1 {{'@objc' instance method in extension of subclass of 'ResilientNSObjectOutsideParent' requires}}
}

extension AnotherResilientNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
  // expected-error@-1 {{'@objc' instance method in extension of subclass of 'ResilientNSObjectOutsideParent' requires}}
}

// Note: @_fixed_layout on a class only applies to the storage layout and
// not metadata, which remains resilient.

@_fixed_layout
@objc public class FixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}
// expected-error@-1 {{explicit '@objc' on subclass of 'FixedLayoutNSObjectOutsideParent' requires}}

@_fixed_layout
public class AnotherFixedLayoutNSObjectSubclass : FixedLayoutNSObjectOutsideParent {}

extension FixedLayoutNSObjectOutsideParent {
  @objc public func categoryOneMethod() {}
  // expected-error@-1 {{'@objc' instance method in extension of subclass of 'FixedLayoutNSObjectOutsideParent' requires}}
}

extension AnotherFixedLayoutNSObjectSubclass {
  @objc public func categoryTwoMethod() {}
  // expected-error@-1 {{'@objc' instance method in extension of subclass of 'FixedLayoutNSObjectOutsideParent' requires}}
}

// If either the class or the extension member has sufficiently narrow
// availability, we're okay.
extension AnotherResilientNSObjectSubclass {
  @available(macOS 11, iOS 14.0.0, tvOS 14.0.0, watchOS 7.0.0, *)
  @objc public func availableCategoryOneMethod() {}
}

extension AnotherResilientNSObjectSubclass {
  @available(macOS 11, iOS 14.0.0, tvOS 14.0.0, watchOS 7.0.0, *)
  @objc public func availableCategoryTwoMethod() {}
}

@available(macOS 11, iOS 14.0.0, tvOS 14.0.0, watchOS 7.0.0, *)
@objc public class AvailableResilientNSObjectSubclass : ResilientNSObjectOutsideParent {}

@available(macOS 11, iOS 14.0.0, tvOS 14.0.0, watchOS 7.0.0, *)
extension AnotherResilientNSObjectSubclass {
  @objc public func categoryThreeMethod() {}
}
