// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -application-extension -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -target %target-cpu-apple-macosx51 -print-ast %s | %FileCheck %s
// RUN: %target-swift-frontend -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -target %target-cpu-apple-macosx14 -print-ast %s | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_DeriveConformancesViaMacros

// CHECK-LABEL: internal enum HasElementsWithAvailability : Hashable
enum HasElementsWithAvailability: Hashable {
  // CHECK:    @_semantics("derived_enum_equals") @_implements(Equatable, ==(_:_:)) internal static func __derived_enum_equals(_ lhs: `Self`, _ rhs: `Self`) -> Bool {
  // CHECK-NEXT:    var index_lhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch lhs {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_lhs = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:    fatalError("Unavailable code reached")  
  // CHECK-NEXT:    case .obsoleted50:
  // CHECK-NEXT:      index_lhs = 1
  // CHECK-NEXT:    case .introduced50:
  // CHECK-NEXT:      index_lhs = 2
  // CHECK-NEXT:    case .unavailableMacOSAppExtension:
  // CHECK-NEXT:      index_lhs = 3
  // CHECK-NEXT:    }
  // CHECK-NEXT:    var index_rhs: Int
  // CHECK-EMPTY:
  // CHECK-NEXT:    switch rhs {
  // CHECK-NEXT:    case .alwaysAvailable:
  // CHECK-NEXT:      index_rhs = 0
  // CHECK-NEXT:    case .neverAvailable:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .unavailableMacOS:
  // CHECK-NEXT:    fatalError("Unavailable code reached")
  // CHECK-NEXT:    case .obsoleted50:
  // CHECK-NEXT:      index_rhs = 1
  // CHECK-NEXT:    case .introduced50:
  // CHECK-NEXT:      index_rhs = 2
  // CHECK-NEXT:    case .unavailableMacOSAppExtension:
  // CHECK-NEXT:      index_rhs = 3
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
