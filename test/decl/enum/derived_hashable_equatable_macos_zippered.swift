// RUN: %target-swift-frontend -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -print-ast %s | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK-LABEL: internal enum HasElementsWithAvailability : Hashable
enum HasElementsWithAvailability: Hashable {
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
  // CHECK:       @available(iOS, unavailable)
  // CHECK-NEXT:  case unavailableiOS
  @available(iOS, unavailable)
  case unavailableiOS
  // CHECK:       @available(macCatalyst, unavailable)
  // CHECK-NEXT:  case unavailableMacCatalyst
  @available(macCatalyst, unavailable)
  case unavailableMacCatalyst
  // CHECK:       @available(macOS, unavailable)
  // CHECK-NEXT:  @available(iOS, unavailable)
  // CHECK-NEXT:  case unavailableMacOSAndiOS
  @available(macOS, unavailable)
  @available(iOS, unavailable)
  case unavailableMacOSAndiOS
  // CHECK:       @available(macOS, unavailable)
  // CHECK-NEXT:  @available(macCatalyst, unavailable)
  // CHECK-NEXT:  case unavailableMacOSAndMacCatalyst
  @available(macOS, unavailable)
  @available(macCatalyst, unavailable)
  case unavailableMacOSAndMacCatalyst

  // CHECK:       @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: HasElementsWithAvailability, _ b: HasElementsWithAvailability) -> Bool {
  // CHECK-NEXT:    var index_a: Int
  // CHECK-NEXT:    switch a {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_a = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      index_a = 1
  // CHECK-NEXT:    case .unavailableiOS:
  // CHECK-NEXT:      index_a = 2
  // CHECK-NEXT:    case .unavailableMacCatalyst:
  // CHECK-NEXT:      index_a = 3
  // CHECK-NEXT:    case .unavailableMacOSAndiOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOSAndMacCatalyst:
  // CHECK-NEXT:      index_a = 4
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_b: Int
  // CHECK-NEXT:    switch b {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_b = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      index_b = 1
  // CHECK-NEXT:    case .unavailableiOS:
  // CHECK-NEXT:      index_b = 2
  // CHECK-NEXT:    case .unavailableMacCatalyst:
  // CHECK-NEXT:      index_b = 3
  // CHECK-NEXT:    case .unavailableMacOSAndiOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOSAndMacCatalyst:
  // CHECK-NEXT:      index_b = 4
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_a == index_b
  // CHECK-NEXT:  }

  // CHECK:       internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:    var discriminator: Int
  // CHECK-NEXT:    switch self {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      discriminator = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      discriminator = 1
  // CHECK-NEXT:    case .unavailableiOS:
  // CHECK-NEXT:      discriminator = 2
  // CHECK-NEXT:    case .unavailableMacCatalyst:
  // CHECK-NEXT:      discriminator = 3
  // CHECK-NEXT:    case .unavailableMacOSAndiOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached{{.*}}
  // CHECK-NEXT:    case .unavailableMacOSAndMacCatalyst:
  // CHECK-NEXT:      discriminator = 4
  // CHECK-NEXT:    }
  // CHECK-NEXT:    hasher.combine(discriminator)
  // CHECK-NEXT:  }

  // CHECK:       internal var hashValue: Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return _hashValue(for: self)
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
}
