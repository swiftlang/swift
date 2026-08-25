// Mock SDK will contain an appropriate SDKSettings and a loadable stdlib
// RUN: %empty-directory(%t/mock-sdk)
// RUN: %empty-directory(%t/mock-sdk/usr/lib/swift)
// RUN: cp -r %test-resource-dir/xros/Swift.swiftmodule %t/mock-sdk/usr/lib/swift/Swift.swiftmodule
// RUN: cp %S/Inputs/XROS1.0.sdk/SDKSettings.json %t/mock-sdk/SDKSettings.json

// DEFINE: %{args} = \
// DEFINE:   -parse-as-library -module-name main \
// DEFINE:   -target arm64-apple-xros1.0 -sdk %t/mock-sdk \
// DEFINE:   -I %t/mock-sdk/usr/lib/swift/

// RUN: %swift -print-ast %s -verify %{args} | %FileCheck %s

// Also make sure that lowering the synthesized code doesn't crash.
// RUN: %swift -emit-sil -o /dev/null %s -verify %{args}
// RUN: %swift -emit-sil -o /dev/null %s -verify %{args} -enable-library-evolution

// REQUIRES: OS=xros
// REQUIRES: SWIFT_STDLIB_ARCH=arm64

// CHECK-LABEL:  public enum RawValueEnum : Int {

public enum RawValueEnum: Int {
  case alwaysAvailable = 0

  @available(visionOS 1.1, *)
  case introducedAfterDeployment = 1

  @available(visionOS, unavailable)
  case unavailableOnVisionOS = 2

  @available(*, unavailable)
  case universallyUnavailable = 3

  @available(visionOS, obsoleted: 1.0)
  case alreadyObsolete = 4

  @available(visionOS, deprecated: 1.0)
  case alreadyDeprecated = 5

  @available(iOS 1.0, *)
  case introducedIniOS1 = 6

  @available(iOS 16.0, *)
  case introducedIniOS16 = 7

  @available(iOS 17.0, *)
  case introducedIniOS17 = 8

  @available(iOS 18.0, *)
  case introducedIniOS18 = 9

  @available(iOS 99.0, *)
  case introducedIniOS99 = 10

  @available(iOS 999.9.9, *)
  case introducedIniOS999 = 11

  @available(iOS, unavailable)
  case unavailableOniOS = 12

  @available(iOS 18.0, visionOS 1.1, *)
  case introducedIniOS18AndVisionOS1_1 = 13

  @available(macOS 15.0, *)
  case introducedOnUnrelatedPlatform = 14

  // CHECK-LABEL:   @inlinable public init?(rawValue: Int) {
  // CHECK-NEXT:     switch rawValue {
  // CHECK-NEXT:     case 0:
  // CHECK-NEXT:       self = RawValueEnum.alwaysAvailable
  // CHECK-NEXT:     case 1:
  // CHECK-NEXT:       guard #available(visionOS 1.1, *) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.introducedAfterDeployment
  // CHECK-NEXT:     case 5:
  // CHECK-NEXT:       self = RawValueEnum.alreadyDeprecated
  // CHECK-NEXT:     case 6:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS1
  // CHECK-NEXT:     case 7:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS16
  // CHECK-NEXT:     case 8:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS17
  // CHECK-NEXT:     case 9:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS18
  // CHECK-NEXT:     case 10:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS99
  // CHECK-NEXT:     case 11:
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS999
  // CHECK-NEXT:     case 13:
  // CHECK-NEXT:       guard #available(visionOS 1.1, *) else {
  // CHECK-NEXT:         return nil
  // CHECK-NEXT:       }
  // CHECK-NEXT:       self = RawValueEnum.introducedIniOS18AndVisionOS1_1
  // CHECK-NEXT:     case 14:
  // CHECK-NEXT:       self = RawValueEnum.introducedOnUnrelatedPlatform
  // CHECK-NEXT:     default:
  // CHECK-NEXT:       return nil
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }

  // CHECK-LABEL:   public var rawValue: Int {
  // CHECK-NEXT:     @inlinable get {
  // CHECK-NEXT:       switch self {
  // CHECK-NEXT:       case .alwaysAvailable:
  // CHECK-NEXT:         return 0
  // CHECK-NEXT:       case .introducedAfterDeployment:
  // CHECK-NEXT:         return 1
  // CHECK-NEXT:       case .unavailableOnVisionOS:
  // CHECK-NEXT:         return 2
  // CHECK-NEXT:       case .universallyUnavailable:
  // CHECK-NEXT:         return 3
  // CHECK-NEXT:       case .alreadyObsolete:
  // CHECK-NEXT:         return 4
  // CHECK-NEXT:       case .alreadyDeprecated:
  // CHECK-NEXT:         return 5
  // CHECK-NEXT:       case .introducedIniOS1:
  // CHECK-NEXT:         return 6
  // CHECK-NEXT:       case .introducedIniOS16:
  // CHECK-NEXT:         return 7
  // CHECK-NEXT:       case .introducedIniOS17:
  // CHECK-NEXT:         return 8
  // CHECK-NEXT:       case .introducedIniOS18:
  // CHECK-NEXT:         return 9
  // CHECK-NEXT:       case .introducedIniOS99:
  // CHECK-NEXT:         return 10
  // CHECK-NEXT:       case .introducedIniOS999:
  // CHECK-NEXT:         return 11
  // CHECK-NEXT:       case .unavailableOniOS:
  // CHECK-NEXT:         return 12
  // CHECK-NEXT:       case .introducedIniOS18AndVisionOS1_1:
  // CHECK-NEXT:         return 13
  // CHECK-NEXT:       case .introducedOnUnrelatedPlatform:
  // CHECK-NEXT:         return 14
  // CHECK-NEXT:       }
  // CHECK-NEXT:     }
  // CHECK-NEXT:   }
}

// CHECK-LABEL:  public enum NoPayloadEnum : Hashable {

public enum NoPayloadEnum: Hashable {
  case alwaysAvailable

  @available(visionOS 1.1, *)
  case introducedAfterDeployment

  @available(visionOS, unavailable)
  case unavailableOnVisionOS

  @available(iOS, unavailable)
  case unavailableOniOS

  @available(iOS 17.0, *)
  case introducedIniOS17

  // CHECK-LABEL:   @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) public static func __derived_enum_equals(_ a: NoPayloadEnum, _ b: NoPayloadEnum) -> Bool {
  // CHECK-NEXT:     var index_a: Int
  // CHECK-NEXT:     switch a {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_a = 0
  // CHECK-NEXT:     case .introducedAfterDeployment:
  // CHECK-NEXT:       index_a = 1
  // CHECK-NEXT:     case .unavailableOnVisionOS:
  // CHECK-NEXT:       index_a = 2
  // CHECK-NEXT:     case .unavailableOniOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .introducedIniOS17:
  // CHECK-NEXT:       index_a = 3
  // CHECK-NEXT:     }
  // CHECK-NEXT:     var index_b: Int
  // CHECK-NEXT:     switch b {
  // CHECK-NEXT:     case .alwaysAvailable:
  // CHECK-NEXT:       index_b = 0
  // CHECK-NEXT:     case .introducedAfterDeployment:
  // CHECK-NEXT:       index_b = 1
  // CHECK-NEXT:     case .unavailableOnVisionOS:
  // CHECK-NEXT:       index_b = 2
  // CHECK-NEXT:     case .unavailableOniOS:
  // CHECK-NEXT:       _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:     case .introducedIniOS17:
  // CHECK-NEXT:       index_b = 3
  // CHECK-NEXT:     }
  // CHECK-NEXT:     return index_a == index_b
  // CHECK-NEXT:   }
}
