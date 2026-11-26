// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL,CHECK-NORMAL-PRE50,CHECK-NORMAL-NO-APPEXT
// RUN: %target-swift-frontend -application-extension -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL,CHECK-NORMAL-PRE50
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx51 -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL,CHECK-NORMAL-NO-APPEXT
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx14 -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-NORMAL,CHECK-NORMAL-PRE50,CHECK-NORMAL-NO-APPEXT

// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-MACRO,CHECK-MACRO-PRE50,CHECK-MACRO-NO-APPEXT
// RUN: %target-swift-frontend -application-extension -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-MACRO,CHECK-MACRO-PRE50
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx51 -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-MACRO,CHECK-MACRO-NO-APPEXT
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx14 -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-MACRO,CHECK-MACRO-PRE50,CHECK-MACRO-NO-APPEXT

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum HasElementsWithAvailability : CodingKey
enum HasElementsWithAvailability: CodingKey {
  // CHECK-NORMAL:       case alwaysAvailable
  // CHECK-NORMAL:       @available(*, unavailable)
  // CHECK-NORMAL-NEXT:  case neverAvailable
  // CHECK-NORMAL:       @available(macOS, unavailable)
  // CHECK-NORMAL-NEXT:  case unavailableMacOS
  // CHECK-NORMAL:       @available(macOS, obsoleted: 50)
  // CHECK-NORMAL-NEXT:  case obsoleted50
  // CHECK-NORMAL:       @available(macOS 50, *)
  // CHECK-NORMAL-NEXT:  case introduced50
  // CHECK-NORMAL:       @available(macOSApplicationExtension, unavailable)
  // CHECK-NORMAL-NEXT:  case unavailableMacOSAppExtension

  // CHECK-NORMAL:                   internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:                switch stringValue {
  // CHECK-NORMAL-NEXT:                case "alwaysAvailable":
  // CHECK-NORMAL-NEXT:                  self = HasElementsWithAvailability.alwaysAvailable
  // CHECK-NORMAL-PRE50-NEXT:          case "obsoleted50":
  // CHECK-NORMAL-PRE50-NEXT:            self = HasElementsWithAvailability.obsoleted50
  // CHECK-NORMAL-NEXT:                case "introduced50":
  // CHECK-NORMAL-NEXT:                  self = HasElementsWithAvailability.introduced50
  // CHECK-NORMAL-NO-APPEXT-NEXT:      case "unavailableMacOSAppExtension":
  // CHECK-NORMAL-NO-APPEXT-NEXT:        self = HasElementsWithAvailability.unavailableMacOSAppExtension
  // CHECK-NORMAL-NEXT:                default:
  // CHECK-NORMAL-NEXT:                  return nil
  // CHECK-NORMAL-NEXT:                }
  // CHECK-NORMAL-NEXT:              }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    return nil
  // CHECK-NEXT:  }

  // CHECK:       internal var intValue: Int? {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return nil
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK-MACRO:                   internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:                switch stringValue {
  // CHECK-MACRO-NEXT:                case "alwaysAvailable":
  // CHECK-MACRO-NEXT:                  self = .alwaysAvailable
  // CHECK-MACRO-PRE50-NEXT:          case "obsoleted50":
  // CHECK-MACRO-PRE50-NEXT:            self = .obsoleted50
  // CHECK-MACRO-NEXT:                case "introduced50":
  // CHECK-MACRO-PRE50-NEXT:            guard #available(macOS 50, *) else {
  // CHECK-MACRO-PRE50-NEXT:              return nil
  // CHECK-MACRO-PRE50-NEXT:            }
  // CHECK-MACRO-NEXT:                  self = .introduced50
  // CHECK-MACRO-NO-APPEXT-NEXT:      case "unavailableMacOSAppExtension":
  // CHECK-MACRO-NO-APPEXT-NEXT:        self = .unavailableMacOSAppExtension
  // CHECK-MACRO-NEXT:                default:
  // CHECK-MACRO-NEXT:                  return nil
  // CHECK-MACRO-NEXT:                }
  // CHECK-MACRO-NEXT:              }

  // CHECK:       internal var stringValue: String {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      switch self {
  // CHECK-NEXT:      case .alwaysAvailable:
  // CHECK-NEXT:        return "alwaysAvailable"
  // CHECK-NEXT:      case .neverAvailable:
  // CHECK-NEXT:        return "neverAvailable"
  // CHECK-NEXT:      case .unavailableMacOS:
  // CHECK-NEXT:        return "unavailableMacOS"
  // CHECK-NEXT:      case .obsoleted50:
  // CHECK-NEXT:        return "obsoleted50"
  // CHECK-NEXT:      case .introduced50:
  // CHECK-NEXT:        return "introduced50"
  // CHECK-NEXT:      case .unavailableMacOSAppExtension:
  // CHECK-NEXT:        return "unavailableMacOSAppExtension"
  // CHECK-NEXT:      }
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK-MACRO:       case alwaysAvailable
  case alwaysAvailable
  // CHECK-MACRO:       @available(*, unavailable)
  // CHECK-MACRO-NEXT:  case neverAvailable
  @available(*, unavailable)
  case neverAvailable
  // CHECK-MACRO:       @available(macOS, unavailable)
  // CHECK-MACRO-NEXT:  case unavailableMacOS
  @available(macOS, unavailable)
  case unavailableMacOS
  // CHECK-MACRO:       @available(macOS, obsoleted: 50)
  // CHECK-MACRO-NEXT:  case obsoleted50
  @available(macOS, obsoleted: 50)
  case obsoleted50
  // CHECK-MACRO:       @available(macOS 50, *)
  // CHECK-MACRO-NEXT:  case introduced50
  @available(macOS, introduced: 50)
  case introduced50
  // CHECK-MACRO:       @available(macOSApplicationExtension, unavailable)
  // CHECK-MACRO-NEXT:  case unavailableMacOSAppExtension
  @available(macOSApplicationExtension, unavailable)
  case unavailableMacOSAppExtension
}

// CHECK-LABEL: internal enum StringRawWithAvailability : String, CodingKey
enum StringRawWithAvailability: String, CodingKey {
  // CHECK-NORMAL:       case alwaysAvailable
  // CHECK-NORMAL:       @available(*, unavailable)
  // CHECK-NORMAL-NEXT:  case neverAvailable
  // CHECK-NORMAL:       @available(macOS 50, *)
  // CHECK-NORMAL-NEXT:  case introduced50

  // CHECK-NORMAL:                internal init?(rawValue: String) {
  // CHECK-NORMAL-NEXT:             switch _findStringSwitchCase(cases: ["a", "i"], string: rawValue) {
  // CHECK-NORMAL-NEXT:             case 0:
  // CHECK-NORMAL-NEXT:               self = StringRawWithAvailability.alwaysAvailable
  // CHECK-NORMAL-NEXT:             case 1:
  // CHECK-NORMAL-PRE50-NEXT:         guard #available(macOS 50, *) else {
  // CHECK-NORMAL-PRE50-NEXT:           return nil
  // CHECK-NORMAL-PRE50-NEXT:         }
  // CHECK-NORMAL-NEXT:               self = StringRawWithAvailability.introduced50
  // CHECK-NORMAL-NEXT:             default:
  // CHECK-NORMAL-NEXT:               return nil
  // CHECK-NORMAL-NEXT:             }
  // CHECK-NORMAL-NEXT:           }

  // CHECK-NORMAL:       internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:    self.init(rawValue: stringValue)
  // CHECK-NORMAL-NEXT:  }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    return nil
  // CHECK-NEXT:  }

  // CHECK-MACRO:       internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:    self.init(rawValue: stringValue)
  // CHECK-MACRO-NEXT:  }

  // CHECK:       internal var stringValue: String {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return self.rawValue
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK-MACRO:       case alwaysAvailable
  case alwaysAvailable = "a"
  // CHECK-MACRO:       @available(*, unavailable)
  // CHECK-MACRO-NEXT:  case neverAvailable
  @available(*, unavailable)
  case neverAvailable = "n"
  // CHECK-MACRO:       @available(macOS 50, *)
  // CHECK-MACRO-NEXT:  case introduced50
  @available(macOS, introduced: 50)
  case introduced50 = "i"
}

// CHECK-LABEL: internal enum IntRawWithAvailability : Int, CodingKey
enum IntRawWithAvailability: Int, CodingKey {
  // CHECK-NORMAL:       case alwaysAvailable
  // CHECK-NORMAL:       @available(macOS, unavailable)
  // CHECK-NORMAL-NEXT:  case unavailableMacOS

  // CHECK-NORMAL:       internal init?(stringValue: String) {
  // CHECK-NORMAL-NEXT:    switch stringValue {
  // CHECK-NORMAL-NEXT:    case "alwaysAvailable":
  // CHECK-NORMAL-NEXT:      self = IntRawWithAvailability.alwaysAvailable
  // CHECK-NORMAL-NEXT:    default:
  // CHECK-NORMAL-NEXT:      return nil
  // CHECK-NORMAL-NEXT:    }
  // CHECK-NORMAL-NEXT:  }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    self.init(rawValue: intValue)
  // CHECK-NEXT:  }

  // CHECK:       internal var intValue: Int? {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return self.rawValue
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK-MACRO:       internal init?(stringValue: String) {
  // CHECK-MACRO-NEXT:    switch stringValue {
  // CHECK-MACRO-NEXT:    case "alwaysAvailable":
  // CHECK-MACRO-NEXT:      self = .alwaysAvailable
  // CHECK-MACRO-NEXT:    default:
  // CHECK-MACRO-NEXT:      return nil
  // CHECK-MACRO-NEXT:    }
  // CHECK-MACRO-NEXT:  }

  // CHECK:       internal var stringValue: String {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      switch self {
  // CHECK-NEXT:      case .alwaysAvailable:
  // CHECK-NEXT:        return "alwaysAvailable"
  // CHECK-NEXT:      case .unavailableMacOS:
  // CHECK-NEXT:        return "unavailableMacOS"
  // CHECK-NEXT:      }
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK-MACRO:       case alwaysAvailable
  case alwaysAvailable = 1
  // CHECK-MACRO:       @available(macOS, unavailable)
  // CHECK-MACRO-NEXT:  case unavailableMacOS
  @available(macOS, unavailable)
  case unavailableMacOS = 2
}
