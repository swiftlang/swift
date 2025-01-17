// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/NormalLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-public-helper.swift
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-helper.swift -I %t

// RUN: %target-typecheck-verify-swift -I %t -enable-library-evolution

@_implementationOnly import BADLibrary
// expected-warning @-1 {{'@_implementationOnly' is deprecated, use 'internal import' instead}}

public struct PublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

@usableFromInline internal struct UFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

public class PublicClassStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

// MARK: Frozen types

@frozen
public struct FrozenPublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

@frozen
@usableFromInline internal struct FrozenUFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

@_fixed_layout
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
public struct FixedLayoutPublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

@_fixed_layout
// expected-warning@-1 {{'@frozen' attribute is now used for fixed-layout structs}}
@usableFromInline internal struct FixedLayoutUFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}

@_fixed_layout
public class FrozenPublicClassStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use struct 'BadStruct' here; 'BADLibrary' has been imported as implementation-only}}
}
