// RUN: %target-swift-frontend -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -application-extension -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx51 -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx14 -print-ast %s | %FileCheck %s
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

  // CHECK:       @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ a: HasElementsWithAvailability, _ b: HasElementsWithAvailability) -> Bool {
  // CHECK-NEXT:    var index_a: Int
  // CHECK-NEXT:    switch a {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_a = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .obsoleted50:
  // CHECK-NEXT:      index_a = 1
  // CHECK-NEXT:    case .introduced50:
  // CHECK-NEXT:      index_a = 2
  // CHECK-NEXT:    case .unavailableMacOSAppExtension:
  // CHECK-NEXT:      index_a = 3
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_b: Int
  // CHECK-NEXT:    switch b {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_b = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .obsoleted50:
  // CHECK-NEXT:      index_b = 1
  // CHECK-NEXT:    case .introduced50:
  // CHECK-NEXT:      index_b = 2
  // CHECK-NEXT:    case .unavailableMacOSAppExtension:
  // CHECK-NEXT:      index_b = 3
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_a == index_b
  // CHECK-NEXT:  }

  // CHECK:       internal func hash(into hasher: inout Hasher) {
  // CHECK-NEXT:    var discriminator: Int
  // CHECK-NEXT:    switch self {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      discriminator = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      _diagnoseUnavailableCodeReached()
  // CHECK-NEXT:    case .obsoleted50:
  // CHECK-NEXT:      discriminator = 1
  // CHECK-NEXT:    case .introduced50:
  // CHECK-NEXT:      discriminator = 2
  // CHECK-NEXT:    case .unavailableMacOSAppExtension:
  // CHECK-NEXT:      discriminator = 3
  // CHECK-NEXT:    }
  // CHECK-NEXT:    hasher.combine(discriminator)
  // CHECK-NEXT:  }

  // CHECK:       internal var hashValue: Int {
  // CHECK-NEXT:    get {
  // CHECK-NEXT:      return _hashValue(for: self)
  // CHECK-NEXT:    }
  // CHECK-NEXT:  }
}
