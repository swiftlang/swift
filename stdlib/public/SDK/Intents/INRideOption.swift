//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS) || os(watchOS)

// Simply extending the INRideOption type doesn't work due to:
// <rdar://problem/29447066>
// Compiler incorrectly handles combinations of availability declarations on
// independent axes.
internal protocol _INRideOptionMeteredFare {
  var __usesMeteredFare: NSNumber? { get set }
}

extension _INRideOptionMeteredFare {
  @available(swift, obsoleted: 4)
  @nonobjc
  public final var usesMeteredFare: NSNumber? {
    get {
      return __usesMeteredFare
    }
    set(newUsesMeteredFare) {
      __usesMeteredFare = newUsesMeteredFare
    }
  }

  @available(swift, introduced: 4.0)
  @nonobjc
  public var usesMeteredFare: Bool? {
    get {
      return __usesMeteredFare?.boolValue
    }
    set(newUsesMeteredFare) {
      __usesMeteredFare = newUsesMeteredFare.map { NSNumber(value: $0) }
    }
  }
}

@available(iOS 10.0, watchOS 3.2, *)
extension INRideOption : _INRideOptionMeteredFare {
}

#endif
