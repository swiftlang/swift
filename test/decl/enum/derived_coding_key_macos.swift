// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50,CHECK-NO-APPEXT
// RUN: %target-swift-frontend -application-extension -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50,CHECK-APPEXT
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx51 -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-POST50,CHECK-NO-APPEXT
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx14 -print-ast %s | %FileCheck %s --check-prefixes=CHECK,CHECK-PRE50,CHECK-NO-APPEXT

// REQUIRES: OS=macosx

// CHECK-LABEL: internal enum HasElementsWithAvailability : CodingKey
enum HasElementsWithAvailability: CodingKey {
  // CHECK:       case alwaysAvailable
  case alwaysAvailable
  // CHECK:       @available(*, unavailable)
  // CHECK-NEXT:  case neverAvailable
  @available(*, unavailable)
  case neverAvailable
  // CHECK:       @available(macOS, unavailable)
  // CHECK-NEXT:  case unavailableMacOS
  @available(macOS, unavailable)
  case unavailableMacOS
  // CHECK:       @available(macOS, obsoleted: 50)
  // CHECK-NEXT:  case obsoleted50
  @available(macOS, obsoleted: 50)
  case obsoleted50
  // CHECK:       @available(macOS 50, *)
  // CHECK-NEXT:  case introduced50
  @available(macOS, introduced: 50)
  case introduced50
  // CHECK:       @available(macOSApplicationExtension, unavailable)
  // CHECK-NEXT:  case unavailableMacOSAppExtension
  @available(macOSApplicationExtension, unavailable)
  case unavailableMacOSAppExtension

  // CHECK:                internal init?(stringValue: String) {
  // CHECK-NEXT:             switch stringValue {
  // CHECK-NEXT:             case "alwaysAvailable":
  // CHECK-NEXT:               self = HasElementsWithAvailability.alwaysAvailable
  // CHECK-PRE50-NEXT:       case "obsoleted50":
  // CHECK-PRE50-NEXT:         self = HasElementsWithAvailability.obsoleted50
  // CHECK-NEXT:             case "introduced50":
  // CHECK-NEXT:               self = HasElementsWithAvailability.introduced50
  // CHECK-NO-APPEXT-NEXT:   case "unavailableMacOSAppExtension":
  // CHECK-NO-APPEXT-NEXT:     self = HasElementsWithAvailability.unavailableMacOSAppExtension
  // CHECK-NEXT:             default:
  // CHECK-NEXT:               return nil
  // CHECK-NEXT:             }
  // CHECK-NEXT:           }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    return nil
  // CHECK-NEXT:  }

  // CHECK:       internal var intValue: Int? {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return nil
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

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
}

// CHECK-LABEL: internal enum StringRawWithAvailability : String, CodingKey
enum StringRawWithAvailability: String, CodingKey {
  // CHECK:       case alwaysAvailable
  case alwaysAvailable = "a"
  // CHECK:       @available(*, unavailable)
  // CHECK-NEXT:  case neverAvailable
  @available(*, unavailable)
  case neverAvailable = "n"
  // CHECK:       @available(macOS 50, *)
  // CHECK-NEXT:  case introduced50
  @available(macOS, introduced: 50)
  case introduced50 = "i"

  // CHECK:                internal init?(rawValue: String) {
  // CHECK-NEXT:             switch _findStringSwitchCase(cases: ["a", "i"], string: rawValue) {
  // CHECK-NEXT:             case 0:
  // CHECK-NEXT:               self = StringRawWithAvailability.alwaysAvailable
  // CHECK-NEXT:             case 1:
  // CHECK-PRE50-NEXT:         guard #available(macOS 50, *) else {
  // CHECK-PRE50-NEXT:           return nil
  // CHECK-PRE50-NEXT:         }
  // CHECK-NEXT:               self = StringRawWithAvailability.introduced50
  // CHECK-NEXT:             default:
  // CHECK-NEXT:               return nil
  // CHECK-NEXT:             }
  // CHECK-NEXT:           }

  // CHECK:       internal init?(stringValue: String) {
  // CHECK-NEXT:    self.init(rawValue: stringValue)
  // CHECK-NEXT:  }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    return nil
  // CHECK-NEXT:  }

  // CHECK:       internal var stringValue: String {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return self.rawValue
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
}

// CHECK-LABEL: internal enum IntRawWithAvailability : Int, CodingKey
enum IntRawWithAvailability: Int, CodingKey {
  // CHECK:       case alwaysAvailable
  case alwaysAvailable = 1
  // CHECK:       @available(macOS, unavailable)
  // CHECK-NEXT:  case unavailableMacOS
  @available(macOS, unavailable)
  case unavailableMacOS = 2

  // CHECK:       internal init?(stringValue: String) {
  // CHECK-NEXT:    switch stringValue {
  // CHECK-NEXT:    case "alwaysAvailable":
  // CHECK-NEXT:      self = IntRawWithAvailability.alwaysAvailable
  // CHECK-NEXT:    default:
  // CHECK-NEXT:      return nil
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

  // CHECK:       internal init?(intValue: Int) {
  // CHECK-NEXT:    self.init(rawValue: intValue)
  // CHECK-NEXT:  }

  // CHECK:       internal var intValue: Int? {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return self.rawValue
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }

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
}
