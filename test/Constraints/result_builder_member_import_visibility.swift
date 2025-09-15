// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Transitive.swiftmodule -parse-as-library %t/Transitive.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Direct.swiftmodule -I %t/ -parse-as-library %t/Direct.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t/ %t/Client.swift -verify-additional-prefix no-member-import-
// RUN: %target-swift-frontend -typecheck -verify -I %t/ %t/Client.swift -enable-upcoming-feature MemberImportVisibility -verify-additional-prefix member-import-

// REQUIRES: swift_feature_MemberImportVisibility

//--- Transitive.swift

@resultBuilder
public struct TransitiveIntBuilder {
  public static func buildBlock(_ v: Int) -> Int {
    return v
  }
}

public func transitiveTakesTransitiveBuilder(
  @TransitiveIntBuilder builder: () -> Int
) {
  _ = builder()
}

extension Int {
  public static func transitiveTakesTransitiveBuilder(
    @TransitiveIntBuilder builder: () -> Int
  ) {
    _ = builder()
  }

  public static func ambiguous(
    @TransitiveIntBuilder transitiveBuilder builder: () -> Int
  ) {
    _ = builder()
  }
}

//--- Direct.swift

import Transitive

@resultBuilder
public struct DirectIntBuilder {
  public static func buildBlock(_ v: Int) -> Int {
    return v
  }
}

public func directTakesDirectBuilder(
  @DirectIntBuilder builder: () -> Int
) {
  _ = builder()
}

public func directTakesTransitiveBuilder(
  @TransitiveIntBuilder builder: () -> Int
) {
  _ = builder()
}

extension Int {
  public static func directTakesDirectBuilder(
    @DirectIntBuilder builder: () -> Int
  ) {
    _ = builder()
  }

  public static func directTakesTransitiveBuilder(
    @TransitiveIntBuilder builder: () -> Int
  ) {
    _ = builder()
  }

  public static func ambiguous(
    @DirectIntBuilder directBuilder builder: () -> Int
  ) {
    _ = builder()
  }
}

//--- Client.swift

import Direct

// expected-member-import-note@-1 4 {{add import of module 'Transitive'}}

transitiveTakesTransitiveBuilder { 1 } // expected-error {{cannot find 'transitiveTakesTransitiveBuilder' in scope}}
directTakesDirectBuilder { 1 }
directTakesTransitiveBuilder { 1 } // expected-member-import-error {{static method 'buildBlock' is not available due to missing import of defining module 'Transitive'}}

Int.transitiveTakesTransitiveBuilder { 1 } // expected-member-import-error {{static method 'transitiveTakesTransitiveBuilder(builder:)' is not available due to missing import of defining module 'Transitive'}}
// expected-member-import-error@-1 {{static method 'buildBlock' is not available due to missing import of defining module 'Transitive'}}
Int.directTakesDirectBuilder { 1 }
Int.directTakesTransitiveBuilder { 1 } // expected-member-import-error {{static method 'buildBlock' is not available due to missing import of defining module 'Transitive'}}
Int.ambiguous { 1 } // expected-no-member-import-error {{ambiguous use of 'ambiguous'}}
// expected-no-member-import-note@-1 {{use an explicit argument label instead of a trailing closure to call 'ambiguous(transitiveBuilder:)'}}
// expected-no-member-import-note@-2 {{use an explicit argument label instead of a trailing closure to call 'ambiguous(directBuilder:)'}}
