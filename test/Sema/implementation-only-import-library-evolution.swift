// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/BADLibrary.swiftmodule %S/Inputs/implementation-only-import-in-decls-helper.swift

// RUN: %target-typecheck-verify-swift -I %t -enable-library-evolution

@_implementationOnly import BADLibrary

public struct PublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@usableFromInline internal struct UFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

public class PublicClassStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // okay
  private var privatelyBad: BadStruct? // okay
  private let letIsLikeVar = [BadStruct]() // okay
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

// MARK: Frozen types

@_fixed_layout
public struct FrozenPublicStructStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@_fixed_layout
@usableFromInline internal struct FrozenUFIStructStoredProperties {
  @usableFromInline var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}

@_fixed_layout
public class FrozenPublicClassStoredProperties {
  public var publiclyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  internal var internallyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private var privatelyBad: BadStruct? // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  private let letIsLikeVar: [BadStruct] = [] // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
  
  private var computedIsOkay: BadStruct? { return nil } // okay
  private static var staticIsOkay: BadStruct? // okay
  @usableFromInline internal var computedUFIIsNot: BadStruct? { return nil } // expected-error {{cannot use 'BadStruct' here; 'BADLibrary' has been imported as '@_implementationOnly'}}
}
