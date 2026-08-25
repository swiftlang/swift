// DEFINE: %{args} = -module-name main -target %target-cpu-apple-macosx10.52

// RUN: %target-swift-frontend -print-ast %s -verify %{args} \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,NOEXT
// RUN: %target-swift-frontend -print-ast %s -verify %{args} -application-extension \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,EXT

// Also make sure that lowering the synthesized code doesn't crash.
// RUN: %swift -emit-sil -o /dev/null %s -verify %{args}
// RUN: %swift -emit-sil -o /dev/null %s -verify %{args} -enable-library-evolution

// REQUIRES: OS=macosx

// CHECK-LABEL:  public enum RawValueEnum : Int {

public enum RawValueEnum: Int {
  case alwaysAvailable = 0

  @available(macOS 10.51, *)
  case introducedBeforeDeployment = 1

  @available(macOS 10.55, *)
  case introducedAfterDeployment = 2

  @available(macOSApplicationExtension 10.56, *)
  case introducedAfterDeploymentForAppExtensions = 3

  @available(macOS, unavailable)
  case unavailableOnMacOS = 4

  @available(macOSApplicationExtension, unavailable)
  case unavailableForAppExtensions = 5

  @available(*, unavailable)
  case universallyUnavailable = 6

  @available(macOS, obsoleted: 10.99)
  case notObsoleteYet = 7

  @available(macOS, obsoleted: 10.51)
  case alreadyObsolete = 8

  @available(macOS, deprecated: 10.51)
  case alreadyDeprecated = 9

  @available(macOS, deprecated: 99.0)
  case deprecatedInFuture = 10

  @available(macOS, introduced: 10.55, deprecated: 99.0)
  case introducedAfterDeploymentAndDeprecated = 11

  @available(iOS 18.0, *)
  case introducedOnOtherPlatform = 12

  // NOEXT-LABEL:   public init?(rawValue: Int) {
  // NOEXT-NEXT:     switch rawValue {
  // NOEXT-NEXT:     case 0:
  // NOEXT-NEXT:       self = RawValueEnum.alwaysAvailable
  // NOEXT-NEXT:     case 1:
  // NOEXT-NEXT:       self = RawValueEnum.introducedBeforeDeployment
  // NOEXT-NEXT:     case 2:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = RawValueEnum.introducedAfterDeployment
  // NOEXT-NEXT:     case 3:
  // NOEXT-NEXT:       self = RawValueEnum.introducedAfterDeploymentForAppExtensions
  // NOEXT-NEXT:     case 5:
  // NOEXT-NEXT:       self = RawValueEnum.unavailableForAppExtensions
  // NOEXT-NEXT:     case 7:
  // NOEXT-NEXT:       self = RawValueEnum.notObsoleteYet
  // NOEXT-NEXT:     case 9:
  // NOEXT-NEXT:       self = RawValueEnum.alreadyDeprecated
  // NOEXT-NEXT:     case 10:
  // NOEXT-NEXT:       self = RawValueEnum.deprecatedInFuture
  // NOEXT-NEXT:     case 11:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = RawValueEnum.introducedAfterDeploymentAndDeprecated
  // NOEXT-NEXT:     case 12:
  // NOEXT-NEXT:       self = RawValueEnum.introducedOnOtherPlatform
  // NOEXT-NEXT:     default:
  // NOEXT-NEXT:       return nil
  // NOEXT-NEXT:     }
  // NOEXT-NEXT:   }

  // EXT-LABEL:   public init?(rawValue: Int) {
  // EXT-NEXT:     switch rawValue {
  // EXT-NEXT:     case 0:
  // EXT-NEXT:       self = RawValueEnum.alwaysAvailable
  // EXT-NEXT:     case 1:
  // EXT-NEXT:       self = RawValueEnum.introducedBeforeDeployment
  // EXT-NEXT:     case 2:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = RawValueEnum.introducedAfterDeployment
  // EXT-NEXT:     case 3:
  // EXT-NEXT:       guard #available(macOSApplicationExtension 10.56, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = RawValueEnum.introducedAfterDeploymentForAppExtensions
  // EXT-NEXT:     case 7:
  // EXT-NEXT:       self = RawValueEnum.notObsoleteYet
  // EXT-NEXT:     case 9:
  // EXT-NEXT:       self = RawValueEnum.alreadyDeprecated
  // EXT-NEXT:     case 10:
  // EXT-NEXT:       self = RawValueEnum.deprecatedInFuture
  // EXT-NEXT:     case 11:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = RawValueEnum.introducedAfterDeploymentAndDeprecated
  // EXT-NEXT:     case 12:
  // EXT-NEXT:       self = RawValueEnum.introducedOnOtherPlatform
  // EXT-NEXT:     default:
  // EXT-NEXT:       return nil
  // EXT-NEXT:     }
  // EXT-NEXT:   }

  // CHECK-LABEL:   public var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .alwaysAvailable:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .introducedBeforeDeployment:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .introducedAfterDeployment:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       case .introducedAfterDeploymentForAppExtensions:
  // CHECK-NEXT:         return 3
  // CHECK-NEXT:       case .unavailableOnMacOS:
  // CHECK-NEXT:         return 4
  // CHECK-NEXT:       case .unavailableForAppExtensions:
  // CHECK-NEXT:         return 5
  // CHECK-NEXT:       case .universallyUnavailable:
  // CHECK-NEXT:         return 6
  // CHECK-NEXT:       case .notObsoleteYet:
  // CHECK-NEXT:         return 7
  // CHECK-NEXT:       case .alreadyObsolete:
  // CHECK-NEXT:         return 8
  // CHECK-NEXT:       case .alreadyDeprecated:
  // CHECK-NEXT:         return 9
  // CHECK-NEXT:       case .deprecatedInFuture:
  // CHECK-NEXT:         return 10
  // CHECK-NEXT:       case .introducedAfterDeploymentAndDeprecated:
  // CHECK-NEXT:         return 11
  // CHECK-NEXT:       case .introducedOnOtherPlatform:
  // CHECK-NEXT:         return 12
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum NoPayloadEnum : Hashable {

public enum NoPayloadEnum: Hashable {
  case alwaysAvailable

  @available(macOS 10.55, *)
  case introducedAfterDeployment

  @available(macOS, unavailable)
  case unavailableOnMacOS

  @available(macOSApplicationExtension, unavailable)
  case unavailableForAppExtensions

  @available(*, unavailable)
  case universallyUnavailable

  @available(macOS, obsoleted: 10.51)
  case alreadyObsolete

  @available(macOS, deprecated: 10.51)
  case alreadyDeprecated

  // CHECK-LABEL:   @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) public static func __derived_enum_equals(_ a: NoPayloadEnum, _ b: NoPayloadEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .introducedAfterDeployment:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     case .unavailableOnMacOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableForAppExtensions:
  // CHECK-NEXT:       index_a = 2
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .alreadyObsolete:
  // CHECK-NEXT:       index_a = 3
  // CHECK-NEXT:     case .alreadyDeprecated:
  // CHECK-NEXT:       index_a = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .introducedAfterDeployment:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     case .unavailableOnMacOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableForAppExtensions:
  // CHECK-NEXT:       index_b = 2
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .alreadyObsolete:
  // CHECK-NEXT:       index_b = 3
  // CHECK-NEXT:     case .alreadyDeprecated:
  // CHECK-NEXT:       index_b = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a == index_b
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     var discriminator: Int
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       discriminator = 0
  // CHECK-NEXT:     case .introducedAfterDeployment:
  // CHECK-NEXT:       discriminator = 1
  // CHECK-NEXT:     case .unavailableOnMacOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableForAppExtensions:
  // CHECK-NEXT:       discriminator = 2
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .alreadyObsolete:
  // CHECK-NEXT:       discriminator = 3
  // CHECK-NEXT:     case .alreadyDeprecated:
  // CHECK-NEXT:       discriminator = 4
  // CHECK-NEXT:     }
  // CHECK-NEXT:     hasher.combine(discriminator)
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum PayloadEnum : Hashable {

public enum PayloadEnum: Hashable {
  case alwaysAvailable(Int)

  @available(macOS, unavailable)
  case unavailableOnMacOS(Int)

  @available(macOSApplicationExtension, unavailable)
  case unavailableForAppExtensions(Int)

  @available(*, unavailable)
  case universallyUnavailable(Int)

  @available(macOS, deprecated: 10.51)
  case alreadyDeprecated(Int)

  // CHECK-LABEL:   @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) public static func __derived_enum_equals(_ a: PayloadEnum, _ b: PayloadEnum) -> Bool {
  // CHECK-NEXT:     switch (a, b) {
  // CHECK-NEXT:     case (.alwaysAvailable(let l0), .alwaysAvailable(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.unavailableOnMacOS, .unavailableOnMacOS):
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case (.unavailableForAppExtensions(let l0), .unavailableForAppExtensions(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     case (.universallyUnavailable, .universallyUnavailable):
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case (.alreadyDeprecated(let l0), .alreadyDeprecated(let r0)):
  // CHECK-NEXT:       guard l0 == r0 else {
  // CHECK-NEXT:         return false
  // CHECK-NEXT:       }
  // CHECK-NEXT:       return true
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return false
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:     switch self {
  // CHECK-NEXT:     case .alwaysAvailable(let a0):
  // CHECK-NEXT:       hasher.combine(0)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .unavailableOnMacOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .unavailableForAppExtensions(let a0):
  // CHECK-NEXT:       hasher.combine(1)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     case .universallyUnavailable:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .alreadyDeprecated(let a0):
  // CHECK-NEXT:       hasher.combine(2)
  // CHECK-NEXT:       hasher.combine(a0)
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}
