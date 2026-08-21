// DEFINE: %{args} = \
// DEFINE:   -module-name main \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain \
// DEFINE:   -define-dynamic-availability-domain OtherDynamicDomain

// RUN: %target-swift-frontend -print-ast %s -verify %{args} | %FileCheck %s

// Also make sure that lowering the synthesized code doesn't crash.
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args}
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args} -enable-library-evolution

// REQUIRES: swift_feature_CustomAvailability

// CHECK-LABEL:  public enum RawValueEnum : Int {

public enum RawValueEnum: Int {
  case alwaysAvailable = 0

  @available(EnabledDomain)
  case availableInEnabledDomain = 1

  @available(EnabledDomain, unavailable)
  case unavailableInEnabledDomain = 2

  @available(AlwaysEnabledDomain)
  case availableInAlwaysEnabledDomain = 3

  @available(AlwaysEnabledDomain, unavailable)
  case unavailableInAlwaysEnabledDomain = 4

  @available(DisabledDomain)
  case availableInDisabledDomain = 5

  @available(DisabledDomain, unavailable)
  case unavailableInDisabledDomain = 6

  @available(DynamicDomain)
  case availableInDynamicDomain = 7

  @available(DynamicDomain, unavailable)
  case unavailableInDynamicDomain = 8

  @available(EnabledDomain)
  @available(DynamicDomain)
  case availableInEnabledAndDynamicDomains = 9

  @available(DynamicDomain)
  @available(OtherDynamicDomain)
  case availableInTwoDynamicDomains = 10

  @available(EnabledDomain, deprecated)
  case deprecatedInEnabledDomain = 11

  @available(DisabledDomain, deprecated)
  case deprecatedInDisabledDomain = 12

  @available(DynamicDomain, deprecated, message: "use something else")
  case deprecatedInDynamicDomain = 13

  @available(DynamicDomain)
  @available(EnabledDomain, deprecated)
  case availableInDynamicDomainAndDeprecatedInEnabledDomain = 14

  // CHECK-LABEL:   public init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = RawValueEnum.alwaysAvailable
  // CHECK-NEXT:     case 1:
  // CHECK-NEXT:       guard #available(EnabledDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.availableInEnabledDomain
  // CHECK-NEXT:     case 3:
  // CHECK-NEXT:       self = RawValueEnum.availableInAlwaysEnabledDomain
  // CHECK-NEXT:     case 6:
  // CHECK-NEXT:       guard #unavailable(DisabledDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.unavailableInDisabledDomain
  // CHECK-NEXT:     case 7:
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.availableInDynamicDomain
  // CHECK-NEXT:     case 8:
  // CHECK-NEXT:       guard #unavailable(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.unavailableInDynamicDomain
  // CHECK-NEXT:     case 9:
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard #available(EnabledDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.availableInEnabledAndDynamicDomains
  // CHECK-NEXT:     case 10:
  // CHECK-NEXT:       guard #available(OtherDynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.availableInTwoDynamicDomains
  // CHECK-NEXT:     case 11:
  // CHECK-NEXT:       self = RawValueEnum.deprecatedInEnabledDomain
  // CHECK-NEXT:     case 12:
  // CHECK-NEXT:       self = RawValueEnum.deprecatedInDisabledDomain
  // CHECK-NEXT:     case 13:
  // CHECK-NEXT:       self = RawValueEnum.deprecatedInDynamicDomain
  // CHECK-NEXT:     case 14:
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.availableInDynamicDomainAndDeprecatedInEnabledDomain
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .alwaysAvailable:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .availableInEnabledDomain:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .unavailableInEnabledDomain:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       case .availableInAlwaysEnabledDomain:
  // CHECK-NEXT:         return 3
  // CHECK-NEXT:       case .unavailableInAlwaysEnabledDomain:
  // CHECK-NEXT:         return 4
  // CHECK-NEXT:       case .availableInDisabledDomain:
  // CHECK-NEXT:         return 5
  // CHECK-NEXT:       case .unavailableInDisabledDomain:
  // CHECK-NEXT:         return 6
  // CHECK-NEXT:       case .availableInDynamicDomain:
  // CHECK-NEXT:         return 7
  // CHECK-NEXT:       case .unavailableInDynamicDomain:
  // CHECK-NEXT:         return 8
  // CHECK-NEXT:       case .availableInEnabledAndDynamicDomains:
  // CHECK-NEXT:         return 9
  // CHECK-NEXT:       case .availableInTwoDynamicDomains:
  // CHECK-NEXT:         return 10
  // CHECK-NEXT:       case .deprecatedInEnabledDomain:
  // CHECK-NEXT:         return 11
  // CHECK-NEXT:       case .deprecatedInDisabledDomain:
  // CHECK-NEXT:         return 12
  // CHECK-NEXT:       case .deprecatedInDynamicDomain:
  // CHECK-NEXT:         return 13
  // CHECK-NEXT:       case .availableInDynamicDomainAndDeprecatedInEnabledDomain:
  // CHECK-NEXT:         return 14
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}


// CHECK-LABEL:  public enum NoPayloadEnum : Hashable {

public enum NoPayloadEnum: Hashable {
  case alwaysAvailable

  @available(EnabledDomain)
  case availableInEnabledDomain

  @available(EnabledDomain, unavailable)
  case unavailableInEnabledDomain

  @available(AlwaysEnabledDomain)
  case availableInAlwaysEnabledDomain

  @available(AlwaysEnabledDomain, unavailable)
  case unavailableInAlwaysEnabledDomain

  @available(DisabledDomain)
  case availableInDisabledDomain

  @available(DisabledDomain, unavailable)
  case unavailableInDisabledDomain

  @available(DynamicDomain)
  case availableInDynamicDomain

  @available(DynamicDomain, unavailable)
  case unavailableInDynamicDomain

  // CHECK-LABEL:  {{.*}}public static func __derived_enum_equals(_ a: NoPayloadEnum, _ b: NoPayloadEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .availableInEnabledDomain:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     case .unavailableInEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .availableInAlwaysEnabledDomain:
  // CHECK-NEXT:       index_a = 2
  // CHECK-NEXT:     case .unavailableInAlwaysEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .availableInDisabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableInDisabledDomain:
  // CHECK-NEXT:       index_a = 3
  // CHECK-NEXT:     case .availableInDynamicDomain:
  // CHECK-NEXT:       index_a = 4
  // CHECK-NEXT:     case .unavailableInDynamicDomain:
  // CHECK-NEXT:       index_a = 5
  // CHECK-NEXT:     }
  // CHECK:          return index_a == index_b
  // CHECK-NEXT:   }

  // CHECK-LABEL:  public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     var discriminator: Int
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       discriminator = 0
  // CHECK-NEXT:     case .availableInEnabledDomain:
  // CHECK-NEXT:       discriminator = 1
  // CHECK-NEXT:     case .unavailableInEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .availableInAlwaysEnabledDomain:
  // CHECK-NEXT:       discriminator = 2
  // CHECK-NEXT:     case .unavailableInAlwaysEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .availableInDisabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableInDisabledDomain:
  // CHECK-NEXT:       discriminator = 3
  // CHECK-NEXT:     case .availableInDynamicDomain:
  // CHECK-NEXT:       discriminator = 4
  // CHECK-NEXT:     case .unavailableInDynamicDomain:
  // CHECK-NEXT:       discriminator = 5
  // CHECK-NEXT:     }
  // CHECK-NEXT:     hasher.combine(discriminator)
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum PayloadEnum : Hashable {

public enum PayloadEnum: Hashable {
  case alwaysAvailable(Int)

  @available(EnabledDomain, unavailable)
  case unavailableInEnabledDomain(Int)

  @available(AlwaysEnabledDomain, unavailable)
  case unavailableInAlwaysEnabledDomain(Int)

  @available(DisabledDomain, unavailable)
  case unavailableInDisabledDomain(Int)

  @available(DynamicDomain, unavailable)
  case unavailableInDynamicDomain(Int)

  // CHECK-LABEL:  {{.*}}public static func __derived_enum_equals(_ a: PayloadEnum, _ b: PayloadEnum) -> Bool {
  // CHECK-NEXT:     switch (a, b) {
  // CHECK-NEXT:     case (.alwaysAvailable(let l0), .alwaysAvailable(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.unavailableInEnabledDomain, .unavailableInEnabledDomain):
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case (.unavailableInAlwaysEnabledDomain, .unavailableInAlwaysEnabledDomain):
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case (.unavailableInDisabledDomain(let l0), .unavailableInDisabledDomain(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.unavailableInDynamicDomain(let l0), .unavailableInDynamicDomain(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:  public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable(let a0):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .unavailableInEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableInAlwaysEnabledDomain:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableInDisabledDomain(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .unavailableInDynamicDomain(let a0):
  // CHECK-NEXT:       hasher.combine(2)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}
