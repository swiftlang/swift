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

@_exported import CallKit
import Foundation

@available(iOS 10.0, *)
extension CXProviderConfiguration {
  @nonobjc
  public final var supportedHandleTypes: Set<CXHandle.HandleType> {
    get {
      return Set(__supportedHandleTypes.map {
        CXHandle.HandleType(rawValue: $0.intValue)!
      })
    }
    set {
      __supportedHandleTypes =
        Set(newValue.lazy.map { $0.rawValue as NSNumber })
    }
  }
}
