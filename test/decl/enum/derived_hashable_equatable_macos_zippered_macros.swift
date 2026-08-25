// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -target %target-cpu-apple-macosx13 -target-variant %target-cpu-apple-ios16-macabi -print-ast %s | %FileCheck %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum HasElementsWithAvailability : Hashable
enum HasElementsWithAvailability: Hashable {
  // CHECK:       @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_lhs = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      index_lhs = 1
  // CHECK-NEXT:    case .unavailableiOS:
  // CHECK-NEXT:      index_lhs = 2
  // CHECK-NEXT:    case .unavailableMacCatalyst:
  // CHECK-NEXT:      index_lhs = 3
  // CHECK-NEXT:    case .unavailableMacOSAndiOS:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOSAndMacCatalyst:
  // CHECK-NEXT:      index_lhs = 4
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_rhs = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:      index_rhs = 1
  // CHECK-NEXT:    case .unavailableiOS:
  // CHECK-NEXT:      index_rhs = 2
  // CHECK-NEXT:    case .unavailableMacCatalyst:
  // CHECK-NEXT:      index_rhs = 3
  // CHECK-NEXT:    case .unavailableMacOSAndiOS:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOSAndMacCatalyst:
  // CHECK-NEXT:      index_rhs = 4
  // CHECK-NEXT:    }
  // CHECK-NEXT:    return index_lhs == index_rhs
  // CHECK-NEXT:  }

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
