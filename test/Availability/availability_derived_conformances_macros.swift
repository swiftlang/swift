// DEFINE: %{args} = -module-name main -parse-as-library -swift-version 5 \
// DEFINE:   -target %target-cpu-apple-macosx10.52 \
// DEFINE:   -enable-experimental-feature CustomAvailability \
// DEFINE:   -define-enabled-availability-domain EnabledDomain \
// DEFINE:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// DEFINE:   -define-disabled-availability-domain DisabledDomain \
// DEFINE:   -define-dynamic-availability-domain DynamicDomain \
// DEFINE:   -define-dynamic-availability-domain OtherDynamicDomain \
// DEFINE:   -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -print-ast %s -verify %{args} -enable-experimental-feature DeriveConformancesViaMacros > %t/macros.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,RAW < %t/macros.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,INIT,NOEXT < %t/macros.txt
// RUN: %target-swift-frontend -print-ast %s -verify %{args} -enable-experimental-feature DeriveConformancesViaMacros -application-extension > %t/macros-ext.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,RAW < %t/macros-ext.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,INIT,EXT < %t/macros-ext.txt
// RUN: %target-swift-frontend -print-ast %s -verify %{args} > %t/legacy.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,RAW < %t/legacy.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,INIT,LEGACY,NOEXT < %t/legacy.txt
// RUN: %target-swift-frontend -print-ast %s -verify %{args} -application-extension > %t/legacy-ext.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,RAW < %t/legacy-ext.txt
// RUN: %FileCheck %s --check-prefixes=CHECK,INIT,LEGACY,EXT < %t/legacy-ext.txt
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args} -enable-experimental-feature DeriveConformancesViaMacros
// RUN: %target-swift-frontend -emit-sil -o /dev/null %s -verify %{args} -enable-experimental-feature DeriveConformancesViaMacros -enable-library-evolution
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

  // RAW-LABEL:   var rawValue: Int {
  // RAW-NEXT:     get {
  // RAW-NEXT:       switch self {
  // RAW-NEXT:       case .alwaysAvailable:
  // RAW-NEXT:         return 0
  // RAW-NEXT:       case .introducedBeforeDeployment:
  // RAW-NEXT:         return 1
  // RAW-NEXT:       case .introducedAfterDeployment:
  // RAW-NEXT:         return 2
  // RAW-NEXT:       case .introducedAfterDeploymentForAppExtensions:
  // RAW-NEXT:         return 3
  // RAW-NEXT:       case .unavailableOnMacOS:
  // RAW-NEXT:         return 4
  // RAW-NEXT:       case .unavailableForAppExtensions:
  // RAW-NEXT:         return 5
  // RAW-NEXT:       case .universallyUnavailable:
  // RAW-NEXT:         return 6
  // RAW-NEXT:       case .notObsoleteYet:
  // RAW-NEXT:         return 7
  // RAW-NEXT:       case .alreadyObsolete:
  // RAW-NEXT:         return 8
  // RAW-NEXT:       case .alreadyDeprecated:
  // RAW-NEXT:         return 9
  // RAW-NEXT:       case .introducedAfterDeploymentAndDeprecated:
  // RAW-NEXT:         return 10
  // RAW-NEXT:       case .introducedOnOtherPlatform:
  // RAW-NEXT:         return 11
  // RAW-NEXT:       }
  // RAW-NEXT:     }
  // RAW-NEXT:   }

  // NOEXT-LABEL:   init?(rawValue: Int) {
  // NOEXT-NEXT:     switch rawValue {
  // NOEXT-NEXT:     case 0:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.alwaysAvailable
  // NOEXT-NEXT:     case 1:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.introducedBeforeDeployment
  // NOEXT-NEXT:     case 2:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeployment
  // NOEXT-NEXT:     case 3:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeploymentForAppExtensions
  // NOEXT-NEXT:     case 5:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.unavailableForAppExtensions
  // NOEXT-NEXT:     case 7:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.notObsoleteYet
  // NOEXT-NEXT:     case 9:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.alreadyDeprecated
  // NOEXT-NEXT:     case 10:
  // NOEXT-NEXT:       guard #available(macOS 10.55, *) else {
  // NOEXT-NEXT:         return nil
  // NOEXT-NEXT:       }
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeploymentAndDeprecated
  // NOEXT-NEXT:     case 11:
  // NOEXT-NEXT:       self = {{[A-Za-z]*}}.introducedOnOtherPlatform
  // NOEXT-NEXT:     default:
  // NOEXT-NEXT:       return nil
  // NOEXT-NEXT:     }
  // NOEXT-NEXT:   }

  // EXT-LABEL:   init?(rawValue: Int) {
  // EXT-NEXT:     switch rawValue {
  // EXT-NEXT:     case 0:
  // EXT-NEXT:       self = {{[A-Za-z]*}}.alwaysAvailable
  // EXT-NEXT:     case 1:
  // EXT-NEXT:       self = {{[A-Za-z]*}}.introducedBeforeDeployment
  // EXT-NEXT:     case 2:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeployment
  // EXT-NEXT:     case 3:
  // EXT-NEXT:       guard #available(macOSApplicationExtension 10.56, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeploymentForAppExtensions
  // EXT-NEXT:     case 7:
  // EXT-NEXT:       self = {{[A-Za-z]*}}.notObsoleteYet
  // EXT-NEXT:     case 9:
  // EXT-NEXT:       self = {{[A-Za-z]*}}.alreadyDeprecated
  // EXT-NEXT:     case 10:
  // EXT-NEXT:       guard #available(macOS 10.55, *) else {
  // EXT-NEXT:         return nil
  // EXT-NEXT:       }
  // EXT-NEXT:       self = {{[A-Za-z]*}}.introducedAfterDeploymentAndDeprecated
  // EXT-NEXT:     case 11:
  // EXT-NEXT:       self = {{[A-Za-z]*}}.introducedOnOtherPlatform
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

  // RAW-LABEL:   var rawValue: Int {
  // RAW-NEXT:     get {
  // RAW-NEXT:       switch self {
  // RAW-NEXT:       case .availableInEnabledDomain:
  // RAW-NEXT:         return 0
  // RAW-NEXT:       case .unavailableInEnabledDomain:
  // RAW-NEXT:         return 1
  // RAW-NEXT:       case .availableInDisabledDomain:
  // RAW-NEXT:         return 2
  // RAW-NEXT:       case .unavailableInDisabledDomain:
  // RAW-NEXT:         return 3
  // RAW-NEXT:       case .availableInDynamicDomain:
  // RAW-NEXT:         return 4
  // RAW-NEXT:       case .unavailableInDynamicDomain:
  // RAW-NEXT:         return 5
  // RAW-NEXT:       case .availableInTwoDynamicDomains:
  // RAW-NEXT:         return 6
  // RAW-NEXT:       case .deprecatedInDynamicDomain:
  // RAW-NEXT:         return 7
  // RAW-NEXT:       }
  // RAW-NEXT:     }
  // RAW-NEXT:   }

  // INIT-LABEL:   init?(rawValue: Int) {
  // INIT-NEXT:     switch rawValue {
  // INIT-NEXT:     case 0:
  // LEGACY-NEXT:       guard #available(EnabledDomain) else {
  // LEGACY-NEXT:         return nil
  // LEGACY-NEXT:       }
  // INIT-NEXT:       self = {{[A-Za-z]*}}.availableInEnabledDomain
  // INIT-NEXT:     case 3:
  // LEGACY-NEXT:       guard #unavailable(DisabledDomain) else {
  // LEGACY-NEXT:         return nil
  // LEGACY-NEXT:       }
  // INIT-NEXT:       self = {{[A-Za-z]*}}.unavailableInDisabledDomain
  // INIT-NEXT:     case 4:
  // INIT-NEXT:       guard #available(DynamicDomain) else {
  // INIT-NEXT:         return nil
  // INIT-NEXT:       }
  // INIT-NEXT:       self = {{[A-Za-z]*}}.availableInDynamicDomain
  // INIT-NEXT:     case 5:
  // INIT-NEXT:       guard #unavailable(DynamicDomain) else {
  // INIT-NEXT:         return nil
  // INIT-NEXT:       }
  // INIT-NEXT:       self = {{[A-Za-z]*}}.unavailableInDynamicDomain
  // INIT-NEXT:     case 6:
  // INIT-NEXT:       guard #available(OtherDynamicDomain) else {
  // INIT-NEXT:         return nil
  // INIT-NEXT:       }
  // INIT-NEXT:       guard #available(DynamicDomain) else {
  // INIT-NEXT:         return nil
  // INIT-NEXT:       }
  // INIT-NEXT:       self = {{[A-Za-z]*}}.availableInTwoDynamicDomains
  // INIT-NEXT:     case 7:
  // INIT-NEXT:       self = {{[A-Za-z]*}}.deprecatedInDynamicDomain
  // INIT-NEXT:     default:
  // INIT-NEXT:       return nil
  // INIT-NEXT:     }
  // INIT-NEXT:   }
}

// CHECK-LABEL:  public enum UnavailableEnum : Int {

@available(*, unavailable)
public enum UnavailableEnum: Int {
  case a = 0

  @available(*, unavailable)
  case b = 1

  @available(swift, obsoleted: 4)
  case c = 2

  // RAW-LABEL:   var rawValue: Int {
  // RAW-NEXT:     get {
  // RAW-NEXT:       switch self {
  // RAW-NEXT:       case .a:
  // RAW-NEXT:         return 0
  // RAW-NEXT:       case .b:
  // RAW-NEXT:         return 1
  // RAW-NEXT:       case .c:
  // RAW-NEXT:         return 2
  // RAW-NEXT:       }
  // RAW-NEXT:     }
  // RAW-NEXT:   }

  // INIT-LABEL:   init?(rawValue: Int) {
  // INIT-NEXT:     switch rawValue {
  // INIT-NEXT:     case 0:
  // INIT-NEXT:       self = {{[A-Za-z]*}}.a
  // INIT-NEXT:     default:
  // INIT-NEXT:       return nil
  // INIT-NEXT:     }
  // INIT-NEXT:   }
}
