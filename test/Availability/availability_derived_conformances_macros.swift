// DEFINE: %{args} = -module-name main -parse-as-library -swift-version 5 \
// DEFINE:   -target %target-cpu-apple-macosx10.52 \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain \
// DEFINE:   -define-dynamic-availability-domain OtherDynamicDomain \
// DEFINE:   -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) \
// DEFINE:   -enable-experimental-feature DeriveConformancesViaMacros

// RUN: %target-swift-frontend -print-ast %s -verify %{args} \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,NOEXT
// RUN: %target-swift-frontend -print-ast %s -verify %{args} -application-extension \
// RUN:   | %FileCheck %s --check-prefixes=CHECK,EXT

// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args}
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args} -enable-library-evolution

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_CustomAvailability
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL:  public enum PlatformEnum : Int {
public enum PlatformEnum: Int {
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

  @available(macOS, introduced: 10.55, deprecated: 99.0)
  case introducedAfterDeploymentAndDeprecated = 10

  @available(iOS 18.0, *)
  case introducedOnOtherPlatform = 11

  // CHECK-LABEL:   var rawValue: Int {
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
  // CHECK-NEXT:       case .introducedAfterDeploymentAndDeprecated:
  // CHECK-NEXT:         return 10
  // CHECK-NEXT:       case .introducedOnOtherPlatform:
  // CHECK-NEXT:         return 11
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // NOEXT-LABEL:   init?(rawValue: Int) {
  // NOEXT-NEXT:     switch rawValue {
  // NOEXT-NEXT:     case 0:
  // NOEXT-NEXT:       self = .alwaysAvailable
  // NOEXT-NEXT:     case 1:
  // NOEXT-NEXT:       self = .introducedBeforeDeployment
  // NOEXT-NEXT:     case 2:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = .introducedAfterDeployment
  // NOEXT-NEXT:     case 3:
  // NOEXT-NEXT:       self = .introducedAfterDeploymentForAppExtensions
  // NOEXT-NEXT:     case 5:
  // NOEXT-NEXT:       self = .unavailableForAppExtensions
  // NOEXT-NEXT:     case 7:
  // NOEXT-NEXT:       self = .notObsoleteYet
  // NOEXT-NEXT:     case 9:
  // NOEXT-NEXT:       self = .alreadyDeprecated
  // NOEXT-NEXT:     case 10:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = .introducedAfterDeploymentAndDeprecated
  // NOEXT-NEXT:     case 11:
  // NOEXT-NEXT:       self = .introducedOnOtherPlatform
  // NOEXT-NEXT:     default:
  // NOEXT-NEXT:       return nil
  // NOEXT-NEXT:     }
  // NOEXT-NEXT:   }

  // EXT-LABEL:   init?(rawValue: Int) {
  // EXT-NEXT:     switch rawValue {
  // EXT-NEXT:     case 0:
  // EXT-NEXT:       self = .alwaysAvailable
  // EXT-NEXT:     case 1:
  // EXT-NEXT:       self = .introducedBeforeDeployment
  // EXT-NEXT:     case 2:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = .introducedAfterDeployment
  // EXT-NEXT:     case 3:
  // EXT-NEXT:       guard #available(macOSApplicationExtension 10.56, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = .introducedAfterDeploymentForAppExtensions
  // EXT-NEXT:     case 7:
  // EXT-NEXT:       self = .notObsoleteYet
  // EXT-NEXT:     case 9:
  // EXT-NEXT:       self = .alreadyDeprecated
  // EXT-NEXT:     case 10:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = .introducedAfterDeploymentAndDeprecated
  // EXT-NEXT:     case 11:
  // EXT-NEXT:       self = .introducedOnOtherPlatform
  // EXT-NEXT:     default:
  // EXT-NEXT:       return nil
  // EXT-NEXT:     }
  // EXT-NEXT:   }
}

// CHECK-LABEL:  public enum CustomDomainEnum : Int {
public enum CustomDomainEnum: Int {
  @available(EnabledDomain)
  case availableInEnabledDomain = 0

  @available(EnabledDomain, unavailable)
  case unavailableInEnabledDomain = 1

  @available(DisabledDomain)
  case availableInDisabledDomain = 2

  @available(DisabledDomain, unavailable)
  case unavailableInDisabledDomain = 3

  @available(DynamicDomain)
  case availableInDynamicDomain = 4

  @available(DynamicDomain, unavailable)
  case unavailableInDynamicDomain = 5

  @available(DynamicDomain)
  @available(OtherDynamicDomain)
  case availableInTwoDynamicDomains = 6

  @available(DynamicDomain, deprecated, message: "use something else")
  case deprecatedInDynamicDomain = 7

  // CHECK-LABEL:   var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .availableInEnabledDomain:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .unavailableInEnabledDomain:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .availableInDisabledDomain:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       case .unavailableInDisabledDomain:
  // CHECK-NEXT:         return 3
  // CHECK-NEXT:       case .availableInDynamicDomain:
  // CHECK-NEXT:         return 4
  // CHECK-NEXT:       case .unavailableInDynamicDomain:
  // CHECK-NEXT:         return 5
  // CHECK-NEXT:       case .availableInTwoDynamicDomains:
  // CHECK-NEXT:         return 6
  // CHECK-NEXT:       case .deprecatedInDynamicDomain:
  // CHECK-NEXT:         return 7
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = .availableInEnabledDomain
  // CHECK-NEXT:     case 3:
  // CHECK-NEXT:       self = .unavailableInDisabledDomain
  // CHECK-NEXT:     case 4:
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = .availableInDynamicDomain
  // CHECK-NEXT:     case 5:
  // CHECK-NEXT:       guard #unavailable(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = .unavailableInDynamicDomain
  // CHECK-NEXT:     case 6:
  // CHECK-NEXT:       guard #available(OtherDynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       guard #available(DynamicDomain) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = .availableInTwoDynamicDomains
  // CHECK-NEXT:     case 7:
  // CHECK-NEXT:       self = .deprecatedInDynamicDomain
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum UnavailableEnum : Int {

@available(*, unavailable)
public enum UnavailableEnum: Int {
  case a = 0

  @available(*, unavailable)
  case b = 1

  @available(swift, obsoleted: 4)
  case c = 2

  // CHECK-LABEL:   var rawValue: Int {
  // CHECK-NEXT:     get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .a:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .b:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .c:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = .a
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}
