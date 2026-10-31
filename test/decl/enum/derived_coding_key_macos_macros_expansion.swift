// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50,CHECK-NO-APPEXT
// RUN: %target-swift-frontend -application-extension -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx51 -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-APPEXT
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx14 -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -typecheck -dump-macro-expansions %s 2>&1 | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50,CHECK-NO-APPEXT

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DeriveConformancesViaMacros

enum HasElementsWithAvailability: CodingKey {
  case alwaysAvailable
  @available(*, unavailable)
  case neverAvailable
  @available(macOS, unavailable)
  case unavailableMacOS
  @available(macOS, obsoleted: 50)
  case obsoleted50
  @available(macOS, introduced: 50)
  case introduced50
  @available(macOSApplicationExtension, unavailable)
  case unavailableMacOSAppExtension
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .alwaysAvailable:
// CHECK-NEXT:     return "alwaysAvailable"
// CHECK-NEXT:   case .neverAvailable:
// CHECK-NEXT:     return "neverAvailable"
// CHECK-NEXT:   case .unavailableMacOS:
// CHECK-NEXT:     return "unavailableMacOS"
// CHECK-NEXT:   case .obsoleted50:
// CHECK-NEXT:     return "obsoleted50"
// CHECK-NEXT:   case .introduced50:
// CHECK-NEXT:     return "introduced50"
// CHECK-NEXT:   case .unavailableMacOSAppExtension:
// CHECK-NEXT:     return "unavailableMacOSAppExtension"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:                 init?(stringValue: Swift::String) {
// CHECK-NEXT:              switch stringValue {
// CHECK-NEXT:              case "alwaysAvailable":
// CHECK-NEXT:                self = .alwaysAvailable
// CHECK-PRE50-NEXT:        case "obsoleted50":
// CHECK-PRE50-NEXT:          self = .obsoleted50
// CHECK-NEXT:              case "introduced50":
// CHECK-PRE50-NEXT:          guard #available(macOS 50, *) else {
// CHECK-PRE50-NEXT:            return nil
// CHECK-PRE50-NEXT:          }
// CHECK-NEXT:                self = .introduced50
// CHECK-NO-APPEXT-NEXT:    case "unavailableMacOSAppExtension":
// CHECK-NO-APPEXT-NEXT:      self = .unavailableMacOSAppExtension
// CHECK-NEXT:              default:
// CHECK-NEXT:                return nil
// CHECK-NEXT:              }
// CHECK-NEXT:            }

enum StringRawWithAvailability: String, CodingKey {
  case alwaysAvailable = "a"
  @available(*, unavailable)
  case neverAvailable = "n"
  @available(macOS, introduced: 50)
  case introduced50 = "i"
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   return self.rawValue
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   self.init(rawValue: stringValue)
// CHECK-NEXT: }

enum IntRawWithAvailability: Int, CodingKey {
  case alwaysAvailable = 1
  @available(macOS, unavailable)
  case unavailableMacOS = 2
}

// CHECK:      var stringValue: Swift::String {
// CHECK-NEXT:   switch self {
// CHECK-NEXT:   case .alwaysAvailable:
// CHECK-NEXT:     return "alwaysAvailable"
// CHECK-NEXT:   case .unavailableMacOS:
// CHECK-NEXT:     return "unavailableMacOS"
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      init?(stringValue: Swift::String) {
// CHECK-NEXT:   switch stringValue {
// CHECK-NEXT:   case "alwaysAvailable":
// CHECK-NEXT:     self = .alwaysAvailable
// CHECK-NEXT:   default:
// CHECK-NEXT:     return nil
// CHECK-NEXT:   }
// CHECK-NEXT: }

// CHECK:      var intValue: Swift::Int? {
// CHECK-NEXT:   return self.rawValue
// CHECK-NEXT: }

// CHECK:      init?(intValue: Swift::Int) {
// CHECK-NEXT:   self.init(rawValue: intValue)
// CHECK-NEXT: }
